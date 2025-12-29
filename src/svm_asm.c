/*
StackVM-32 Assembler (v1)
------------------------
Single-file, two-pass assembler for the StackVM-32 ISA + syntax we defined.

Outputs:
  - <output>.zvm  (container: header + code + initial memory image)

Build:
  cc -O2 -std=c11 -Wall -Wextra -o svm_asm svm_asm.c

Usage:
  ./svm_asm input.asm output.zvm

Notes / scope:
  - Two sections: .code and .data
  - Labels in .code become addr32 (absolute byte offsets in code blob)
  - Labels in .data become u32 addresses (byte offsets in memory image)
  - Supports directives: .code .data .org .align .byte .word .ascii .asciz .zero .const .entry .module .end
  - Supports instructions per spec (50 opcodes) with immediates as needed
  - Expressions:  label | number | label +/- number | number +/- number (left-to-right)
    (no parentheses / precedence; intentionally simple)
  - Comments start with ';'
  - Strings use double quotes with C-style escapes: \n \r \t \\ \" \0 \xHH

*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <setjmp.h>

#if defined(_MSC_VER)
#define strcasecmp _stricmp
#endif

// ---------------------------- Utilities ----------------------------

// Best-effort source line tracking for generic die()/die2() calls.
// Updated by the lexer as it scans the input.
static int g_cur_line = 0;

// When assembling under a host (e.g. WebAssembly), we want a non-fatal API.
// The CLI path keeps printing to stderr + exit(1) as before.
static int g_trap_errors = 0;
static jmp_buf g_trap_jmp;

static uint8_t* g_svm_asm_err = NULL;
static uint32_t g_svm_asm_err_len = 0;

static void svm_asm_error_clear_internal(void) {
  free(g_svm_asm_err);
  g_svm_asm_err = NULL;
  g_svm_asm_err_len = 0;
}

static void svm_asm_error_setf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  char tmp[2048];
  int n = vsnprintf(tmp, sizeof(tmp), fmt, ap);
  va_end(ap);

  if (n < 0) {
    svm_asm_error_clear_internal();
    return;
  }

  if ((size_t)n >= sizeof(tmp)) n = (int)sizeof(tmp) - 1;

  svm_asm_error_clear_internal();
  g_svm_asm_err = (uint8_t*)malloc((size_t)n + 1);
  if (!g_svm_asm_err) return;
  memcpy(g_svm_asm_err, tmp, (size_t)n);
  g_svm_asm_err[n] = 0;
  g_svm_asm_err_len = (uint32_t)n;
}

static void die(const char* msg) {
  if (g_trap_errors) {
    if (g_cur_line > 0) svm_asm_error_setf("error: line %d: %s", g_cur_line, msg);
    else svm_asm_error_setf("error: %s", msg);
    longjmp(g_trap_jmp, 1);
  }
  if (g_cur_line > 0) fprintf(stderr, "error: line %d: %s\n", g_cur_line, msg);
  else fprintf(stderr, "error: %s\n", msg);
  exit(1);
}

static void die2(const char* msg, const char* detail) {
  if (g_trap_errors) {
    if (g_cur_line > 0) svm_asm_error_setf("error: line %d: %s: %s", g_cur_line, msg, detail);
    else svm_asm_error_setf("error: %s: %s", msg, detail);
    longjmp(g_trap_jmp, 1);
  }
  if (g_cur_line > 0) fprintf(stderr, "error: line %d: %s: %s\n", g_cur_line, msg, detail);
  else fprintf(stderr, "error: %s: %s\n", msg, detail);
  exit(1);
}

static void* xmalloc(size_t n) {
  void* p = malloc(n);
  if (!p) die("out of memory");
  return p;
}

static void* xrealloc(void* p, size_t n) {
  void* q = realloc(p, n);
  if (!q) die("out of memory");
  return q;
}

static char* xstrdup(const char* s) {
  size_t n = strlen(s);
  char* d = (char*)xmalloc(n + 1);
  memcpy(d, s, n + 1);
  return d;
}

static int is_ident_start(int c) { return isalpha(c) || c == '_'; }
static int is_ident_part(int c)  { return isalnum(c) || c == '_'; }

// ---------------------------- Dynamic buffers ----------------------------

typedef struct {
  uint8_t* data;
  size_t len;
  size_t cap;
} Buf;

static void buf_reserve(Buf* b, size_t add) {
  size_t need = b->len + add;
  if (need <= b->cap) return;
  size_t ncap = b->cap ? b->cap : 256;
  while (ncap < need) ncap *= 2;
  b->data = (uint8_t*)xrealloc(b->data, ncap);
  b->cap = ncap;
}

static void buf_write_u8(Buf* b, uint8_t v) {
  buf_reserve(b, 1);
  b->data[b->len++] = v;
}

static void buf_write_bytes(Buf* b, const void* src, size_t n) {
  if (n == 0) return;
  buf_reserve(b, n);
  memcpy(b->data + b->len, src, n);
  b->len += n;
}

static void buf_write_u16_le(Buf* b, uint16_t v) {
  buf_reserve(b, 2);
  b->data[b->len++] = (uint8_t)(v & 0xFF);
  b->data[b->len++] = (uint8_t)((v >> 8) & 0xFF);
}

static void buf_write_u32_le(Buf* b, uint32_t v) {
  buf_reserve(b, 4);
  b->data[b->len++] = (uint8_t)(v & 0xFF);
  b->data[b->len++] = (uint8_t)((v >> 8) & 0xFF);
  b->data[b->len++] = (uint8_t)((v >> 16) & 0xFF);
  b->data[b->len++] = (uint8_t)((v >> 24) & 0xFF);
}

static void buf_set_u32_le(Buf* b, size_t pos, uint32_t v) {
  if (!b || !b->data) die("internal: patching null buffer");
  if (pos + 4 > b->len) die("internal: patch out of range");
  b->data[pos + 0] = (uint8_t)(v & 0xFF);
  b->data[pos + 1] = (uint8_t)((v >> 8) & 0xFF);
  b->data[pos + 2] = (uint8_t)((v >> 16) & 0xFF);
  b->data[pos + 3] = (uint8_t)((v >> 24) & 0xFF);
}

// Ensure buffer is at least 'n' bytes, fill with zeros if extended
static void buf_ensure_len(Buf* b, size_t n) {
  if (b->len >= n) return;
  buf_reserve(b, n - b->len);
  while (b->len < n) b->data[b->len++] = 0;
}

static void buf_align(Buf* b, size_t align) {
  if (align == 0 || (align & (align - 1)) != 0) die("align must be power of 2");
  size_t rem = b->len % align;
  if (rem) {
    size_t pad = align - rem;
    buf_ensure_len(b, b->len + pad);
  }
}

// ---------------------------- Symbols ----------------------------

typedef enum { SYM_CODE, SYM_DATA, SYM_CONST } SymKind;

typedef struct {
  char* name;
  SymKind kind;
  uint32_t value; // code/data address or constant value
} Sym;

typedef struct {
  Sym* items;
  size_t len;
  size_t cap;
} SymTab;

static void symtab_init(SymTab* t) { memset(t, 0, sizeof(*t)); }

static Sym* symtab_find(SymTab* t, const char* name) {
  for (size_t i = 0; i < t->len; i++) {
    if (strcmp(t->items[i].name, name) == 0) return &t->items[i];
  }
  return NULL;
}

static Sym* symtab_add(SymTab* t, const char* name, SymKind kind, uint32_t value) {
  Sym* existing = symtab_find(t, name);
  if (existing) die2("duplicate symbol", name);
  if (t->len == t->cap) {
    t->cap = t->cap ? t->cap * 2 : 128;
    t->items = (Sym*)xrealloc(t->items, t->cap * sizeof(Sym));
  }
  Sym* s = &t->items[t->len++];
  s->name = xstrdup(name);
  s->kind = kind;
  s->value = value;
  return s;
}

// ---------------------------- Fixups ----------------------------

typedef enum { SEC_CODE, SEC_DATA } Section;

typedef enum { FX_ADDR32_CODE, FX_U32_ANY } FixKind;

typedef struct {
  FixKind kind;
  char* sym;
  int32_t addend;
  int line;
  size_t patch_pos;   // position in current section buffer to patch (u32)
  Section sec;
} Fix;

typedef struct {
  Fix* items;
  size_t len;
  size_t cap;
} Fixups;

static void fixups_add(Fixups* f,
                       FixKind kind,
                       const char* sym,
                       int32_t addend,
                       int line,
                       size_t patch_pos,
                       Section sec) {
  if (f->len == f->cap) {
    f->cap = f->cap ? f->cap * 2 : 128;
    f->items = (Fix*)xrealloc(f->items, f->cap * sizeof(Fix));
  }
  Fix* fx = &f->items[f->len++];
  fx->kind = kind;
  fx->sym = xstrdup(sym);
  fx->addend = addend;
  fx->line = line;
  fx->patch_pos = patch_pos;
  fx->sec = sec;
}



// ---------------------------- Tokenizer ----------------------------

typedef enum {
  TOK_EOF, TOK_EOL,
  TOK_IDENT, TOK_NUMBER, TOK_STRING,
  TOK_COLON, TOK_COMMA,
  TOK_PLUS, TOK_MINUS, TOK_EQUAL,
  TOK_DOT
} TokKind;

typedef struct {
  TokKind kind;
  char* text;     // for IDENT/STRING or raw for NUMBER parsing
  int32_t ival;   // for NUMBER (signed 32-bit)
  int line;
} Tok;

typedef struct {
  const char* src;
  size_t pos;
  int line;
  Tok peek;
  int has_peek;
} Lex;

static void lex_init(Lex* L, const char* src) {
  L->src = src;
  L->pos = 0;
  L->line = 1;
  g_cur_line = 1;
  L->has_peek = 0;
  memset(&L->peek, 0, sizeof(L->peek));
}

static void tok_free(Tok* t) {
  if (t->text) free(t->text);
  t->text = NULL;
}

static int lex_cur(Lex* L) { return (unsigned char)L->src[L->pos]; }
static int lex_nextc(Lex* L) {
  int c = (unsigned char)L->src[L->pos];
  if (c == 0) return 0;
  L->pos++;
  if (c == '\n') L->line++;
  g_cur_line = L->line;
  return c;
}

static void lex_skip_ws(Lex* L) {
  for (;;) {
    int c = lex_cur(L);
    if (c == ' ' || c == '\t' || c == '\r') { L->pos++; continue; }
    // comment
    if (c == ';') {
      while (lex_cur(L) && lex_cur(L) != '\n') L->pos++;
      continue;
    }
    break;
  }
}

static int parse_int32_literal(const char* s, int32_t* out) {
  // supports decimal, 0x..., 0b..., leading '-'
  int neg = 0;
  if (*s == '-') { neg = 1; s++; }
  int base = 10;
  if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) { base = 16; s += 2; }
  else if (s[0] == '0' && (s[1] == 'b' || s[1] == 'B')) { base = 2; s += 2; }
  if (*s == 0) return 0;

  int64_t val = 0;
  for (; *s; s++) {
    int d;
    if (base == 16) {
      if (*s >= '0' && *s <= '9') d = *s - '0';
      else if (*s >= 'a' && *s <= 'f') d = 10 + (*s - 'a');
      else if (*s >= 'A' && *s <= 'F') d = 10 + (*s - 'A');
      else return 0;
    } else if (base == 10) {
      if (*s >= '0' && *s <= '9') d = *s - '0';
      else return 0;
    } else {
      if (*s == '0') d = 0;
      else if (*s == '1') d = 1;
      else return 0;
    }
    val = val * base + d;
    if (val > (int64_t)0x7fffffff + (neg ? 1 : 0)) {
      // allow -2147483648
      return 0;
    }
  }
  if (neg) val = -val;
  *out = (int32_t)val;
  return 1;
}

static int read_escape(Lex* L) {
  int c = lex_nextc(L);
  if (c == 0) die("unterminated string escape");
  switch (c) {
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\t';
    case '\\': return '\\';
    case '"': return '"';
    case '0': return '\0';
    case 'x': {
      int h1 = lex_nextc(L), h2 = lex_nextc(L);
      if (!isxdigit(h1) || !isxdigit(h2)) die("bad \\xHH escape");
      int v1 = isdigit(h1) ? h1 - '0' : (tolower(h1) - 'a' + 10);
      int v2 = isdigit(h2) ? h2 - '0' : (tolower(h2) - 'a' + 10);
      return (v1 << 4) | v2;
    }
    default:
      die("unknown escape");
      return 0;
  }
}

static Tok lex_read(Lex* L) {
  if (L->has_peek) {
    L->has_peek = 0;
    return L->peek;
  }

  lex_skip_ws(L);

  Tok t;
  memset(&t, 0, sizeof(t));
  t.line = L->line;
  g_cur_line = t.line;

  int c = lex_cur(L);
  if (c == 0) { t.kind = TOK_EOF; return t; }
  if (c == '\n') { lex_nextc(L); t.kind = TOK_EOL; return t; }

  // single-char tokens
  if (c == ':') { lex_nextc(L); t.kind = TOK_COLON; return t; }
  if (c == ',') { lex_nextc(L); t.kind = TOK_COMMA; return t; }
  if (c == '+') { lex_nextc(L); t.kind = TOK_PLUS; return t; }
  if (c == '-') { lex_nextc(L); t.kind = TOK_MINUS; return t; }
  if (c == '=') { lex_nextc(L); t.kind = TOK_EQUAL; return t; }
  if (c == '.') { lex_nextc(L); t.kind = TOK_DOT; return t; }

  // string
  if (c == '"') {
    lex_nextc(L); // consume "
    char buf[4096];
    size_t n = 0;
    for (;;) {
      int ch = lex_cur(L);
      if (ch == 0 || ch == '\n') die("unterminated string");
      if (ch == '"') { lex_nextc(L); break; }
      if (ch == '\\') { lex_nextc(L); ch = read_escape(L); }
      else { lex_nextc(L); }
      if (n + 1 >= sizeof(buf)) die("string too long");
      buf[n++] = (char)ch;
    }
    buf[n] = 0;
    t.kind = TOK_STRING;
    t.text = xstrdup(buf);
    return t;
  }

  // identifier
  if (is_ident_start(c)) {
    char buf[256];
    size_t n = 0;
    while (is_ident_part(lex_cur(L))) {
      if (n + 1 >= sizeof(buf)) die("identifier too long");
      buf[n++] = (char)lex_nextc(L);
    }
    buf[n] = 0;
    t.kind = TOK_IDENT;
    t.text = xstrdup(buf);
    return t;
  }

  // number literal (including 0x, 0b)
  if (isdigit(c)) {
    char buf[256];
    size_t n = 0;
    while (isxdigit(lex_cur(L)) || lex_cur(L) == 'x' || lex_cur(L) == 'X' || lex_cur(L) == 'b' || lex_cur(L) == 'B') {
      if (n + 1 >= sizeof(buf)) die("number literal too long");
      buf[n++] = (char)lex_nextc(L);
      // stop if next is not valid for current style; keep simple and let parser validate
      int nc = lex_cur(L);
      if (!(isalnum(nc))) break;
    }
    buf[n] = 0;
    int32_t val;
    if (!parse_int32_literal(buf, &val)) die2("bad number literal", buf);
    t.kind = TOK_NUMBER;
    t.ival = val;
    t.text = xstrdup(buf);
    return t;
  }

  // negative number literal begins with '-' handled as TOK_MINUS, parser can combine with number
  die("unexpected character");
  t.kind = TOK_EOF;
  return t;
}

static Tok lex_peek(Lex* L) {
  if (!L->has_peek) {
    L->peek = lex_read(L);
    L->has_peek = 1;
  }
  return L->peek;
}

// ---------------------------- ISA table ----------------------------

typedef enum { OP_NONE, OP_U8, OP_U16, OP_S16, OP_U32, OP_ADDR32 } OpArg;

typedef struct {
  const char* name;
  uint8_t opcode;
  OpArg arg;
} Instr;

static const Instr INSTRS[] = {
  // Control
  {"NOP", 0x00, OP_NONE},
  {"HALT", 0x01, OP_NONE},
  {"SYSCALL", 0x02, OP_U8},
  {"TRAP", 0x03, OP_U16},

  // Branch
  {"JMP", 0x04, OP_ADDR32},
  {"JZ",  0x05, OP_ADDR32},
  {"JNZ", 0x06, OP_ADDR32},

  // Stack
  {"PUSHI", 0x07, OP_U32},
  {"POP",   0x08, OP_NONE},
  {"DUP",   0x09, OP_NONE},
  {"DUP2",  0x0A, OP_NONE},
  {"SWAP",  0x0B, OP_NONE},
  {"ROT",   0x0C, OP_NONE},
  {"OVER",  0x0D, OP_NONE},

  // Calls & frames
  {"CALL",  0x0E, OP_ADDR32},
  {"RET",   0x0F, OP_U8},
  {"ENTER", 0x10, OP_U16},
  {"LEAVE", 0x11, OP_NONE},
  {"LDFP",  0x12, OP_S16},
  {"STFP",  0x13, OP_S16},

  // Memory
  {"LOAD32",  0x14, OP_NONE},
  {"STORE32", 0x15, OP_NONE},
  {"LOAD8U",  0x16, OP_NONE},
  {"STORE8",  0x17, OP_NONE},
  {"MEMCPY",  0x18, OP_NONE},

  // Arithmetic & bitwise
  {"ADD",  0x19, OP_NONE},
  {"SUB",  0x1A, OP_NONE},
  {"MUL",  0x1B, OP_NONE},
  {"DIVS", 0x1C, OP_NONE},
  {"NEG",  0x1D, OP_NONE},
  {"AND",  0x1E, OP_NONE},
  {"OR",   0x1F, OP_NONE},
  {"XOR",  0x20, OP_NONE},
  {"SHL",  0x21, OP_NONE},
  {"SHR",  0x22, OP_NONE},

  // Compare
  {"EQ", 0x23, OP_NONE},
  {"LT", 0x24, OP_NONE},
  {"GT", 0x25, OP_NONE},
  {"LE", 0x26, OP_NONE},
  {"GE", 0x27, OP_NONE},

  // Convenience
  {"ADDI",     0x28, OP_S16},
  {"SUBI",     0x29, OP_S16},
  {"INC",      0x2A, OP_NONE},
  {"DEC",      0x2B, OP_NONE},
  {"MODS",     0x2C, OP_NONE},
  {"NOT",      0x2D, OP_NONE},
  {"CALLI",    0x2E, OP_NONE},
  {"TAILCALL", 0x2F, OP_ADDR32},
  {"LOAD_OFF",  0x30, OP_S16},
  {"STORE_OFF", 0x31, OP_S16},
};

static const size_t INSTRS_N = sizeof(INSTRS)/sizeof(INSTRS[0]);

static const Instr* find_instr(const char* name) {
  for (size_t i = 0; i < INSTRS_N; i++) {
    if (strcasecmp(INSTRS[i].name, name) == 0) return &INSTRS[i];
  }
  return NULL;
}

// ---------------------------- Assembler core ----------------------------

typedef struct {
  SymTab syms;
  Fixups fixups;

  Buf code;
  Buf data;

  Section sec;
  int pass; // 1 or 2 (we actually do two logical passes by replaying parse twice)

  // Optional entry point (written into ZVM header entry_ip)
  int entry_is_set;
  int entry_has_symbol;
  char entry_sym[256];
  int32_t entry_addend;
  int32_t entry_imm;
  int entry_line;

  // Optional linear memory total size (written into ZVM header mem_total_size)
  int mem_total_is_set;
  int mem_total_has_symbol;
  char mem_total_sym[256];
  int32_t mem_total_addend;
  int32_t mem_total_imm;
  int mem_total_line;
} Asm;

static void asm_init(Asm* A) {
  memset(A, 0, sizeof(*A));
  symtab_init(&A->syms);
  A->sec = SEC_CODE;
}

static Buf* cur_buf(Asm* A) { return (A->sec == SEC_CODE) ? &A->code : &A->data; }
static void emit_u8(Asm* A, uint8_t v) { buf_write_u8(cur_buf(A), v); }
static void emit_u16(Asm* A, uint16_t v) { buf_write_u16_le(cur_buf(A), v); }
static void emit_u32(Asm* A, uint32_t v) { buf_write_u32_le(cur_buf(A), v); }
static uint32_t cur_off(Asm* A) { return (uint32_t)cur_buf(A)->len; }
static void set_org(Asm* A, uint32_t off) { buf_ensure_len(cur_buf(A), off); }
static void do_align(Asm* A, uint32_t align) { buf_align(cur_buf(A), align); }

// ---------------------------- Expression parsing ----------------------------

typedef struct {
  int is_symbol;      // 1 if symbol reference
  char sym[256];
  int32_t value;      // if immediate numeric
} Term;

static Term parse_term(Lex* L) {
  Tok t = lex_read(L);
  Term term;
  memset(&term, 0, sizeof(term));

  // allow unary minus for numbers/idents: "-" NUMBER / "-" IDENT (produces addend)
  int sign = 1;
  if (t.kind == TOK_MINUS) {
    tok_free(&t);
    sign = -1;
    t = lex_read(L);
  }

  if (t.kind == TOK_NUMBER) {
    term.is_symbol = 0;
    term.value = (int32_t)(sign * t.ival);
    tok_free(&t);
    return term;
  }
  if (t.kind == TOK_IDENT) {
    term.is_symbol = 1;
    if (strlen(t.text) >= sizeof(term.sym)) die("symbol too long in expression");
    strcpy(term.sym, t.text);
    term.value = 0;
    if (sign == -1) {
      // "-label" is allowed but means "0 - label" which we can't represent in our simple fixup model.
      // Keep it simple: disallow.
      die("'-label' not supported (use label - const)");
    }
    tok_free(&t);
    return term;
  }

  die("expected number or identifier in expression");
  return term;
}

// expression: term ((+|-) number)?  OR number ((+|-) number)?  OR label ((+|-) number)?
// returns: either immediate value (no symbol), or symbol + addend
typedef struct {
  int has_symbol;
  char sym[256];
  int32_t addend;
  int32_t imm; // if !has_symbol
} Expr;

static Expr parse_expr(Lex* L) {
  Expr e;
  memset(&e, 0, sizeof(e));

  Term t1 = parse_term(L);
  Tok op = lex_peek(L);
  int has_op = (op.kind == TOK_PLUS || op.kind == TOK_MINUS);

  if (!has_op) {
    if (t1.is_symbol) {
      e.has_symbol = 1;
      strcpy(e.sym, t1.sym);
      e.addend = 0;
    } else {
      e.has_symbol = 0;
      e.imm = t1.value;
    }
    return e;
  }

  // consume operator
  op = lex_read(L);
  Tok t2tok = lex_peek(L);

  // term2 must be NUMBER for our simple model (label +/- number)
  Term t2 = parse_term(L);
  if (t2.is_symbol) die("only 'label +/- number' supported (second term must be number)");

  int32_t delta = t2.value;
  if (op.kind == TOK_MINUS) delta = -delta;
  tok_free(&op);

  if (t1.is_symbol) {
    e.has_symbol = 1;
    strcpy(e.sym, t1.sym);
    e.addend = delta;
  } else {
    // number +/- number
    e.has_symbol = 0;
    e.imm = t1.value + delta;
  }
  (void)t2tok;
  return e;
}

// ---------------------------- Directives ----------------------------

static void expect_eol_or_eof(Lex* L) {
  Tok t = lex_read(L);
  if (t.kind != TOK_EOL && t.kind != TOK_EOF) die("expected end of line");
  tok_free(&t);
}

// ---------------------------- Instruction emission ----------------------------

static void require_range_s16(int32_t v, int line) {
  if (v < -32768 || v > 32767) {
    g_cur_line = line;
    char buf[64];
    snprintf(buf, sizeof(buf), "%d", v);
    die2("s16 out of range", buf);
  }
}
static void require_range_u16(int32_t v, int line) {
  if (v < 0 || v > 65535) {
    g_cur_line = line;
    char buf[64];
    snprintf(buf, sizeof(buf), "%d", v);
    die2("u16 out of range", buf);
  }
}
static void require_range_u8(int32_t v, int line) {
  if (v < 0 || v > 255) {
    g_cur_line = line;
    char buf[64];
    snprintf(buf, sizeof(buf), "%d", v);
    die2("u8 out of range", buf);
  }
}


// ---------------------------- Fixup resolution ----------------------------

static uint32_t resolve_symbol_value(Asm* A, FixKind kind, const char* sym, int line) {
  Sym* s = symtab_find(&A->syms, sym);
  if (!s) {
    g_cur_line = line;
    die2("undefined symbol", sym);
  }
  if (kind == FX_ADDR32_CODE) {
    if (s->kind != SYM_CODE) {
      g_cur_line = line;
      die2("symbol is not a code label", sym);
    }
    return s->value;
  }
  // FX_U32_ANY: accept CONST, DATA, CODE
  return s->value;
}

static void apply_fixups(Asm* A) {
  for (size_t i = 0; i < A->fixups.len; i++) {
    Fix* fx = &A->fixups.items[i];
    uint32_t base = resolve_symbol_value(A, fx->kind, fx->sym, fx->line);
    int64_t val64 = (int64_t)base + (int64_t)fx->addend;
    if (val64 < 0 || val64 > 0xFFFFFFFFLL) {
      g_cur_line = fx->line;
      die2("fixup value out of u32 range for", fx->sym);
    }
    uint32_t val = (uint32_t)val64;

    Buf* b = (fx->sec == SEC_CODE) ? &A->code : &A->data;
    buf_set_u32_le(b, fx->patch_pos, val);
  }
}

static void parse_directive(Asm* A, Lex* L) {
  Tok name = lex_read(L);
  if (name.kind != TOK_IDENT) die("expected directive name after '.'");

  if (strcasecmp(name.text, "code") == 0) { A->sec = SEC_CODE; tok_free(&name); expect_eol_or_eof(L); return; }
  if (strcasecmp(name.text, "data") == 0) { A->sec = SEC_DATA; tok_free(&name); expect_eol_or_eof(L); return; }

  if (strcasecmp(name.text, "entry") == 0) {
    Expr e = parse_expr(L);
    if (A->pass == 1) {
      if (A->entry_is_set) die("duplicate .entry");
      A->entry_is_set = 1;
      A->entry_line = name.line;
      if (e.has_symbol) {
        A->entry_has_symbol = 1;
        if (strlen(e.sym) >= sizeof(A->entry_sym)) die(".entry symbol too long");
        strcpy(A->entry_sym, e.sym);
        A->entry_addend = e.addend;
      } else {
        A->entry_has_symbol = 0;
        A->entry_imm = e.imm;
      }
    }
    tok_free(&name);
    expect_eol_or_eof(L);
    return;
  }

  if (strcasecmp(name.text, "memtotal") == 0) {
    Expr e = parse_expr(L);
    if (A->pass == 1) {
      if (A->mem_total_is_set) die("duplicate .memtotal");
      A->mem_total_is_set = 1;
      A->mem_total_line = name.line;
      if (e.has_symbol) {
        A->mem_total_has_symbol = 1;
        if (strlen(e.sym) >= sizeof(A->mem_total_sym)) die(".memtotal symbol too long");
        strcpy(A->mem_total_sym, e.sym);
        A->mem_total_addend = e.addend;
      } else {
        A->mem_total_has_symbol = 0;
        A->mem_total_imm = e.imm;
      }
    }
    tok_free(&name);
    expect_eol_or_eof(L);
    return;
  }

  if (strcasecmp(name.text, "org") == 0) {
    Expr e = parse_expr(L);
    if (e.has_symbol) die(".org requires immediate value (not label)");
    if (e.imm < 0) die(".org cannot be negative");
    set_org(A, (uint32_t)e.imm);
    tok_free(&name); expect_eol_or_eof(L); return;
  }
  if (strcasecmp(name.text, "align") == 0) {
    Expr e = parse_expr(L);
    if (e.has_symbol) die(".align requires immediate value");
    if (e.imm <= 0) die(".align must be positive");
    do_align(A, (uint32_t)e.imm);
    tok_free(&name); expect_eol_or_eof(L); return;
  }

  if (strcasecmp(name.text, "byte") == 0) {
    tok_free(&name);
    for (;;) {
      Expr e = parse_expr(L);
      if (e.has_symbol) die(".byte does not accept labels");
      if (e.imm < -128 || e.imm > 255) die(".byte value out of range");
      emit_u8(A, (uint8_t)(e.imm & 0xFF));
      Tok t = lex_peek(L);
      if (t.kind == TOK_COMMA) { t = lex_read(L); tok_free(&t); continue; }
      break;
    }
    expect_eol_or_eof(L); return;
  }

  if (strcasecmp(name.text, "word") == 0) {
    tok_free(&name);
    for (;;) {
      Expr e = parse_expr(L);
      if (e.has_symbol) {
        size_t patch_pos = cur_buf(A)->len;
        emit_u32(A, 0);
        if (A->pass == 1) fixups_add(&A->fixups, FX_U32_ANY, e.sym, e.addend, L->line, patch_pos, A->sec);
      } else {
        emit_u32(A, (uint32_t)e.imm);
      }
      Tok t = lex_peek(L);
      if (t.kind == TOK_COMMA) { t = lex_read(L); tok_free(&t); continue; }
      break;
    }
    expect_eol_or_eof(L); return;
  }

  if (strcasecmp(name.text, "zero") == 0) {
    Expr e = parse_expr(L);
    if (e.has_symbol) die(".zero requires immediate value");
    if (e.imm < 0) die(".zero cannot be negative");
    Buf* b = cur_buf(A);
    buf_ensure_len(b, b->len + (size_t)e.imm);
    tok_free(&name); expect_eol_or_eof(L); return;
  }

  if (strcasecmp(name.text, "ascii") == 0 || strcasecmp(name.text, "asciz") == 0) {
    int add_nul = (strcasecmp(name.text, "asciz") == 0);
    Tok s = lex_read(L);
    if (s.kind != TOK_STRING) die("expected string after .ascii/.asciz");
    for (size_t i = 0; i < strlen(s.text); i++) emit_u8(A, (uint8_t)s.text[i]);
    if (add_nul) emit_u8(A, 0);
    tok_free(&s); tok_free(&name); expect_eol_or_eof(L); return;
  }

  if (strcasecmp(name.text, "const") == 0) {
    Tok sym = lex_read(L);
    if (sym.kind != TOK_IDENT) die("expected identifier after .const");
    Tok eq = lex_read(L);
    if (eq.kind != TOK_EQUAL) die("expected '=' in .const");
    tok_free(&eq);
    Expr e = parse_expr(L);
    if (e.has_symbol) die(".const value must be immediate");
    if (A->pass == 1) symtab_add(&A->syms, sym.text, SYM_CONST, (uint32_t)e.imm);
    tok_free(&sym); tok_free(&name); expect_eol_or_eof(L); return;
  }

  if (strcasecmp(name.text, "module") == 0) {
    Tok t = lex_peek(L);
    if (t.kind == TOK_STRING) { t = lex_read(L); tok_free(&t); }
    tok_free(&name); expect_eol_or_eof(L); return;
  }

  if (strcasecmp(name.text, "end") == 0) {
    tok_free(&name);
    Tok t = lex_read(L);
    while (t.kind != TOK_EOL && t.kind != TOK_EOF) { tok_free(&t); t = lex_read(L); }
    tok_free(&t);
    return;
  }

  die2("unknown directive", name.text);
}

static void parse_instruction(Asm* A, Lex* L, Tok mnemonic) {
  const Instr* ins = find_instr(mnemonic.text);
  if (!ins) die2("unknown instruction", mnemonic.text);

  emit_u8(A, ins->opcode);

  if (ins->arg == OP_NONE) {
    tok_free(&mnemonic);
    expect_eol_or_eof(L);
    return;
  }

  Expr e = parse_expr(L);
  Tok t = lex_peek(L);
  if (t.kind == TOK_COMMA) die("too many operands");

  int line = mnemonic.line;
  tok_free(&mnemonic);

  switch (ins->arg) {
    case OP_U8:
      if (e.has_symbol) die("u8 operand cannot be label");
      require_range_u8(e.imm, line);
      emit_u8(A, (uint8_t)e.imm);
      break;
    case OP_U16:
      if (e.has_symbol) die("u16 operand cannot be label");
      require_range_u16(e.imm, line);
      emit_u16(A, (uint16_t)e.imm);
      break;
    case OP_S16:
      if (e.has_symbol) die("s16 operand cannot be label");
      require_range_s16(e.imm, line);
      emit_u16(A, (uint16_t)(int16_t)e.imm);
      break;
    case OP_U32: {
      size_t patch_pos = cur_buf(A)->len;
      emit_u32(A, 0);
      if (e.has_symbol) {
        if (A->pass == 1) fixups_add(&A->fixups, FX_U32_ANY, e.sym, e.addend, line, patch_pos, A->sec);
      } else {
        // patch now (since we already emitted placeholder)
        Buf* b = cur_buf(A);
        buf_set_u32_le(b, patch_pos, (uint32_t)e.imm);
      }
      break;
    }
    case OP_ADDR32: {
      size_t patch_pos = cur_buf(A)->len;
      emit_u32(A, 0);
      if (e.has_symbol) {
        if (A->pass == 1) fixups_add(&A->fixups, FX_ADDR32_CODE, e.sym, e.addend, line, patch_pos, A->sec);
      } else {
        if (e.imm < 0) die("addr32 cannot be negative");
        Buf* b = cur_buf(A);
        buf_set_u32_le(b, patch_pos, (uint32_t)e.imm);
      }
      break;
    }
    default:
      die("internal: bad op arg");
  }

  expect_eol_or_eof(L);
}

static void parse_file(Asm* A, const char* src_text) {
  Lex L;
  lex_init(&L, src_text);

  for (;;) {
    Tok t = lex_read(&L);
    if (t.kind == TOK_EOF) { tok_free(&t); break; }
    if (t.kind == TOK_EOL) { tok_free(&t); continue; }

    if (t.kind == TOK_DOT) {
      tok_free(&t);
      parse_directive(A, &L);
      continue;
    }

    if (t.kind == TOK_IDENT) {
      Tok p = lex_peek(&L);
      if (p.kind == TOK_COLON) {
        Tok colon = lex_read(&L);
        tok_free(&colon);

        if (A->pass == 1) {
          uint32_t addr = cur_off(A);
          SymKind k = (A->sec == SEC_CODE) ? SYM_CODE : SYM_DATA;
          symtab_add(&A->syms, t.text, k, addr);
        }

        tok_free(&t);

        Tok nxt = lex_peek(&L);
        if (nxt.kind == TOK_EOL) continue;
        if (nxt.kind == TOK_EOF) break;

        t = lex_read(&L);
        if (t.kind == TOK_DOT) { tok_free(&t); parse_directive(A, &L); continue; }
        if (t.kind == TOK_IDENT) {
          if (A->sec != SEC_CODE) die("instructions only allowed in .code section");
          parse_instruction(A, &L, t);
          continue;
        }
        die("unexpected token after label");
      }

      if (A->sec != SEC_CODE) die("instructions only allowed in .code section");
      parse_instruction(A, &L, t);
      continue;
    }

    die("unexpected token");
  }
}

// ---------------------------- IO ----------------------------

static char* read_entire_file(const char* path) {
  FILE* f = fopen(path, "rb");
  if (!f) die2("cannot open input", path);
  fseek(f, 0, SEEK_END);
  long n = ftell(f);
  if (n < 0) die("ftell failed");
  fseek(f, 0, SEEK_SET);
  char* buf = (char*)xmalloc((size_t)n + 1);
  size_t rd = fread(buf, 1, (size_t)n, f);
  fclose(f);
  if (rd != (size_t)n) die("read failed");
  buf[n] = 0;
  return buf;
}

// ---------------------------- ZVM Container ----------------------------
// File format (little-endian), header is 28 bytes:
//
//  0  char[4]  magic  = "ZVM1"
//  4  u16      version= 1
//  6  u16      flags  = 0
//  8  u32      code_size
// 12  u32      mem_init_size
// 16  u32      mem_total_size
// 20  u32      entry_ip
// 24  u32      reserved = 0
// 28  u8[]     code bytes
// ..  u8[]     mem init bytes
//
static void write_zvm(const char* path,
                      const uint8_t* code, uint32_t code_len,
                      const uint8_t* mem,  uint32_t mem_len,
                      uint32_t mem_total,
                      uint32_t entry_ip) {
  FILE* f = fopen(path, "wb");
  if (!f) die2("cannot open output", path);

  // Header
  const uint8_t magic[4] = { 'Z','V','M','1' };
  uint16_t version = 1;
  uint16_t flags = 0;
  uint32_t reserved = 0;

  // write fields little-endian explicitly for portability
  if (fwrite(magic, 1, 4, f) != 4) die2("write failed", path);

  uint8_t hdr[24];
  // pack remaining 24 bytes: version, flags, code_len, mem_len, mem_total, entry_ip, reserved
  // little-endian
  hdr[0] = (uint8_t)(version & 0xFF);
  hdr[1] = (uint8_t)((version >> 8) & 0xFF);
  hdr[2] = (uint8_t)(flags & 0xFF);
  hdr[3] = (uint8_t)((flags >> 8) & 0xFF);

  uint32_t fields[5] = { code_len, mem_len, mem_total, entry_ip, reserved };
  for (int i = 0; i < 5; i++) {
    uint32_t v = fields[i];
    hdr[4 + i*4 + 0] = (uint8_t)(v & 0xFF);
    hdr[4 + i*4 + 1] = (uint8_t)((v >> 8) & 0xFF);
    hdr[4 + i*4 + 2] = (uint8_t)((v >> 16) & 0xFF);
    hdr[4 + i*4 + 3] = (uint8_t)((v >> 24) & 0xFF);
  }

  if (fwrite(hdr, 1, 24, f) != 24) die2("write failed", path);

  if (code_len && fwrite(code, 1, code_len, f) != code_len) die2("write failed", path);
  if (mem_len  && fwrite(mem,  1, mem_len,  f) != mem_len)  die2("write failed", path);

  fclose(f);
}

static void write_zvm_to_buf(Buf* out,
                             const uint8_t* code, uint32_t code_len,
                             const uint8_t* mem,  uint32_t mem_len,
                             uint32_t mem_total,
                             uint32_t entry_ip) {
  if (!out) die("internal: null out buffer");
  out->len = 0;

  const uint8_t magic[4] = { 'Z','V','M','1' };
  uint16_t version = 1;
  uint16_t flags = 0;
  uint32_t reserved = 0;

  buf_write_bytes(out, magic, 4);

  uint8_t hdr[24];
  hdr[0] = (uint8_t)(version & 0xFF);
  hdr[1] = (uint8_t)((version >> 8) & 0xFF);
  hdr[2] = (uint8_t)(flags & 0xFF);
  hdr[3] = (uint8_t)((flags >> 8) & 0xFF);

  uint32_t fields[5] = { code_len, mem_len, mem_total, entry_ip, reserved };
  for (int i = 0; i < 5; i++) {
    uint32_t v = fields[i];
    hdr[4 + i*4 + 0] = (uint8_t)(v & 0xFF);
    hdr[4 + i*4 + 1] = (uint8_t)((v >> 8) & 0xFF);
    hdr[4 + i*4 + 2] = (uint8_t)((v >> 16) & 0xFF);
    hdr[4 + i*4 + 3] = (uint8_t)((v >> 24) & 0xFF);
  }

  buf_write_bytes(out, hdr, 24);
  buf_write_bytes(out, code, code_len);
  buf_write_bytes(out, mem, mem_len);
}

// ---------------------------- In-memory assembly API ----------------------------

static uint8_t* g_svm_asm_out = NULL;
static uint32_t g_svm_asm_out_len = 0;

static void svm_asm_output_clear_internal(void) {
  free(g_svm_asm_out);
  g_svm_asm_out = NULL;
  g_svm_asm_out_len = 0;
}

static void asm_free_all(Asm* A) {
  if (!A) return;

  free(A->code.data);
  free(A->data.data);

  for (size_t i = 0; i < A->syms.len; i++) free(A->syms.items[i].name);
  free(A->syms.items);

  for (size_t i = 0; i < A->fixups.len; i++) free(A->fixups.items[i].sym);
  free(A->fixups.items);

  memset(A, 0, sizeof(*A));
}

static void assemble_text_to_container_buf(const char* text, Buf* out_container) {
  Asm A;
  asm_init(&A);

  // Pass 1
  A.pass = 1;
  parse_file(&A, text);

  // Pass 2 (rebuild blobs)
  Buf code1 = A.code, data1 = A.data;
  A.code.data = NULL; A.code.len = A.code.cap = 0;
  A.data.data = NULL; A.data.len = A.data.cap = 0;

  A.sec = SEC_CODE;
  A.pass = 2;
  parse_file(&A, text);

  apply_fixups(&A);

  uint32_t entry_ip = 0u;
  if (A.entry_is_set) {
    if (A.entry_has_symbol) {
      uint32_t base = resolve_symbol_value(&A, FX_ADDR32_CODE, A.entry_sym, A.entry_line);
      int64_t v = (int64_t)base + (int64_t)A.entry_addend;
      if (v < 0 || v > 0xFFFFFFFFLL) {
        g_cur_line = A.entry_line;
        die(".entry out of u32 range");
      }
      entry_ip = (uint32_t)v;
    } else {
      if (A.entry_imm < 0) {
        g_cur_line = A.entry_line;
        die(".entry cannot be negative");
      }
      entry_ip = (uint32_t)A.entry_imm;
    }
    if (entry_ip > (uint32_t)A.code.len) {
      g_cur_line = A.entry_line;
      die(".entry out of range");
    }
  }

  free(code1.data);
  free(data1.data);

  uint32_t mem_total = (A.data.len < 65536u) ? 65536u : (uint32_t)A.data.len;
  if (A.mem_total_is_set) {
    int64_t v = 0;
    if (A.mem_total_has_symbol) {
      Sym* s = symtab_find(&A.syms, A.mem_total_sym);
      if (!s) {
        g_cur_line = A.mem_total_line;
        die2("undefined symbol in .memtotal", A.mem_total_sym);
      }
      if (s->kind != SYM_CONST) {
        g_cur_line = A.mem_total_line;
        die2(".memtotal symbol must be a .const", A.mem_total_sym);
      }
      v = (int64_t)s->value + (int64_t)A.mem_total_addend;
    } else {
      v = (int64_t)A.mem_total_imm;
    }

    if (v <= 0 || v > 0xFFFFFFFFLL) {
      g_cur_line = A.mem_total_line;
      die(".memtotal out of u32 range");
    }
    if ((uint64_t)v < (uint64_t)A.data.len) {
      g_cur_line = A.mem_total_line;
      die(".memtotal must be >= mem_init_size");
    }
    mem_total = (uint32_t)v;
  }

  write_zvm_to_buf(out_container,
                   A.code.data, (uint32_t)A.code.len,
                   A.data.data, (uint32_t)A.data.len,
                   mem_total,
                   entry_ip);

  asm_free_all(&A);
}

// Assemble from a source buffer in memory.
// Returns 1 on success, 0 on error. On success, use svm_asm_output_ptr/len.
int svm_asm_assemble_from_buffer(const uint8_t* src, uint32_t src_len) {
  svm_asm_output_clear_internal();
  svm_asm_error_clear_internal();
  g_cur_line = 0;

  g_trap_errors = 1;
  if (setjmp(g_trap_jmp) != 0) {
    // error already captured
    g_trap_errors = 0;
    return 0;
  }

  char* text = (char*)xmalloc((size_t)src_len + 1);
  memcpy(text, src, (size_t)src_len);
  text[src_len] = 0;

  Buf out = {0};
  assemble_text_to_container_buf(text, &out);

  free(text);

  g_svm_asm_out = out.data;
  g_svm_asm_out_len = (uint32_t)out.len;

  g_trap_errors = 0;
  return 1;
}

uint32_t svm_asm_output_ptr(void) {
  return (uint32_t)(uintptr_t)g_svm_asm_out;
}

uint32_t svm_asm_output_len(void) {
  return g_svm_asm_out_len;
}

void svm_asm_output_clear(void) {
  svm_asm_output_clear_internal();
}

uint32_t svm_asm_error_ptr(void) {
  return (uint32_t)(uintptr_t)g_svm_asm_err;
}

uint32_t svm_asm_error_len(void) {
  return g_svm_asm_err_len;
}

void svm_asm_error_clear(void) {
  svm_asm_error_clear_internal();
}

// ---------------------------- Main ----------------------------

int main(int argc, char** argv) {
  if (argc != 3) {
    fprintf(stderr, "usage: %s input.asm output.zvm\n", argv[0]);
    return 2;
  }
  const char* inpath = argv[1];
  const char* outpath = argv[2];

  char* text = read_entire_file(inpath);

  Asm A;
  asm_init(&A);

  // Pass 1: build symbols + fixups, also emit bytes (placeholders where needed)
  A.pass = 1;
  parse_file(&A, text);

  // Pass 2: re-parse to rebuild code/data identically (so offsets match),
  // while not adding new symbols/fixups (we only record those in pass1).
  // Easiest: reset code/data buffers and parse again with pass=2.
  Buf code1 = A.code, data1 = A.data;
  // We must discard the pass1 emitted blobs; rebuild clean for pass2.
  // Keep symbols+fixups from pass1, but reset blobs.
  A.code.data = NULL; A.code.len = A.code.cap = 0;
  A.data.data = NULL; A.data.len = A.data.cap = 0;

  A.sec = SEC_CODE;
  A.pass = 2;
  parse_file(&A, text);

  // Apply fixups to pass2 blobs
  apply_fixups(&A);

  uint32_t entry_ip = 0u;
  if (A.entry_is_set) {
    if (A.entry_has_symbol) {
      // Entry must be a code label (not CONST/DATA)
      uint32_t base = resolve_symbol_value(&A, FX_ADDR32_CODE, A.entry_sym, A.entry_line);
      int64_t v = (int64_t)base + (int64_t)A.entry_addend;
      if (v < 0 || v > 0xFFFFFFFFLL) {
        fprintf(stderr, "error: line %d: .entry out of u32 range\n", A.entry_line);
        return 1;
      }
      entry_ip = (uint32_t)v;
    } else {
      if (A.entry_imm < 0) {
        fprintf(stderr, "error: line %d: .entry cannot be negative\n", A.entry_line);
        return 1;
      }
      entry_ip = (uint32_t)A.entry_imm;
    }
    if (entry_ip > (uint32_t)A.code.len) {
      fprintf(stderr, "error: line %d: .entry out of range (0x%08X > code_size 0x%08X)\n",
              A.entry_line, entry_ip, (uint32_t)A.code.len);
      return 1;
    }
  }

  // Basic sanity: pass1 and pass2 sizes should match (optional)
  if (code1.len != A.code.len || data1.len != A.data.len) {
    fprintf(stderr, "warning: pass1/pass2 size mismatch (code %zu/%zu, data %zu/%zu)\n",
            code1.len, A.code.len, data1.len, A.data.len);
  }
  free(code1.data);
  free(data1.data);

  uint32_t mem_total = (A.data.len < 65536u) ? 65536u : (uint32_t)A.data.len;
  if (A.mem_total_is_set) {
    int64_t v = 0;
    if (A.mem_total_has_symbol) {
      Sym* s = symtab_find(&A.syms, A.mem_total_sym);
      if (!s) {
        fprintf(stderr, "error: line %d: undefined symbol in .memtotal: %s\n", A.mem_total_line, A.mem_total_sym);
        return 1;
      }
      if (s->kind != SYM_CONST) {
        fprintf(stderr, "error: line %d: .memtotal symbol '%s' must be a .const\n", A.mem_total_line, A.mem_total_sym);
        return 1;
      }
      v = (int64_t)s->value + (int64_t)A.mem_total_addend;
    } else {
      v = (int64_t)A.mem_total_imm;
    }

    if (v <= 0 || v > 0xFFFFFFFFLL) {
      fprintf(stderr, "error: line %d: .memtotal out of u32 range\n", A.mem_total_line);
      return 1;
    }
    if ((uint64_t)v < (uint64_t)A.data.len) {
      fprintf(stderr, "error: line %d: .memtotal (%lld) must be >= mem_init_size (%zu)\n",
              A.mem_total_line, (long long)v, A.data.len);
      return 1;
    }
    mem_total = (uint32_t)v;
  }

  write_zvm(outpath,
            A.code.data, (uint32_t)A.code.len,
            A.data.data, (uint32_t)A.data.len,
            mem_total,
            entry_ip);

  fprintf(stderr, "assembled: %s (code=%zu bytes, mem_init=%zu bytes, mem_total=%u)\n",
          outpath, A.code.len, A.data.len, mem_total);

  free(A.code.data);
  free(A.data.data);

  // free symbols
  for (size_t i = 0; i < A.syms.len; i++) free(A.syms.items[i].name);
  free(A.syms.items);

  // free fixups
  for (size_t i = 0; i < A.fixups.len; i++) free(A.fixups.items[i].sym);
  free(A.fixups.items);

  free(text);
  return 0;
}
