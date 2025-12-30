// ZManLang minimal compiler (v0): ZManLang source -> StackVM-32 assembly for svm_asm
//
// Current implemented subset:
//   - Top-level statements only
//   - print("..."); where ... is a string literal with escapes: \n \t \\ \"
//
// Output:
//   - .asm file compatible with bin/svm_asm
//   - Emits a small __zman_print helper and a main entry.
//
// This is intended as a scaffolding baseline to grow into a full compiler.

#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ------------------------------ Utilities ------------------------------

typedef struct {
  uint8_t* data;
  size_t len;
  size_t cap;
} ByteBuf;

static void die(const char* msg) {
  fprintf(stderr, "zmc: %s\n", msg);
  exit(2);
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

static void bb_init(ByteBuf* b) {
  b->data = NULL;
  b->len = 0;
  b->cap = 0;
}

static void bb_push(ByteBuf* b, uint8_t v) {
  if (b->len == b->cap) {
    size_t new_cap = b->cap ? (b->cap * 2) : 32;
    b->data = (uint8_t*)xrealloc(b->data, new_cap);
    b->cap = new_cap;
  }
  b->data[b->len++] = v;
}

static void bb_free(ByteBuf* b) {
  free(b->data);
  b->data = NULL;
  b->len = 0;
  b->cap = 0;
}

static char* read_entire_file(const char* path, size_t* out_len) {
  FILE* f = fopen(path, "rb");
  if (!f) {
    fprintf(stderr, "zmc: failed to open '%s': %s\n", path, strerror(errno));
    exit(2);
  }

  if (fseek(f, 0, SEEK_END) != 0) {
    fprintf(stderr, "zmc: fseek failed for '%s'\n", path);
    exit(2);
  }
  long end = ftell(f);
  if (end < 0) {
    fprintf(stderr, "zmc: ftell failed for '%s'\n", path);
    exit(2);
  }
  if (fseek(f, 0, SEEK_SET) != 0) {
    fprintf(stderr, "zmc: fseek failed for '%s'\n", path);
    exit(2);
  }

  size_t n = (size_t)end;
  char* buf = (char*)xmalloc(n + 1);
  size_t got = fread(buf, 1, n, f);
  if (got != n) {
    fprintf(stderr, "zmc: failed to read '%s'\n", path);
    exit(2);
  }
  buf[n] = '\0';
  fclose(f);

  if (out_len) *out_len = n;
  return buf;
}

// ------------------------------ Lexer (minimal) ------------------------------

typedef enum {
  TOK_EOF = 0,
  TOK_IDENT,
  TOK_LPAREN,
  TOK_RPAREN,
  TOK_SEMI,
  TOK_STRING,
} TokKind;

typedef struct {
  TokKind kind;
  size_t pos;
  size_t len;
  // for TOK_STRING
  ByteBuf str_bytes;
} Token;

typedef struct {
  const char* src;
  size_t len;
  size_t i;
} Lexer;

static bool is_alpha_(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool is_alnum_(char c) {
  return is_alpha_(c) || (c >= '0' && c <= '9');
}

static void lex_init(Lexer* lx, const char* src, size_t len) {
  lx->src = src;
  lx->len = len;
  lx->i = 0;
}

static void skip_ws_and_comments(Lexer* lx) {
  for (;;) {
    while (lx->i < lx->len) {
      char c = lx->src[lx->i];
      if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
        lx->i++;
        continue;
      }
      break;
    }

    if (lx->i + 1 < lx->len && lx->src[lx->i] == '/' && lx->src[lx->i + 1] == '/') {
      lx->i += 2;
      while (lx->i < lx->len && lx->src[lx->i] != '\n') lx->i++;
      continue;
    }

    return;
  }
}

static void token_free(Token* t) {
  if (t->kind == TOK_STRING) bb_free(&t->str_bytes);
}

static Token next_token(Lexer* lx) {
  skip_ws_and_comments(lx);

  Token t;
  memset(&t, 0, sizeof(t));
  t.kind = TOK_EOF;
  t.pos = lx->i;
  t.len = 0;
  bb_init(&t.str_bytes);

  if (lx->i >= lx->len) {
    t.kind = TOK_EOF;
    return t;
  }

  char c = lx->src[lx->i];

  if (is_alpha_(c)) {
    size_t start = lx->i;
    lx->i++;
    while (lx->i < lx->len && is_alnum_(lx->src[lx->i])) lx->i++;
    t.kind = TOK_IDENT;
    t.pos = start;
    t.len = lx->i - start;
    return t;
  }

  if (c == '(') {
    lx->i++;
    t.kind = TOK_LPAREN;
    t.len = 1;
    return t;
  }
  if (c == ')') {
    lx->i++;
    t.kind = TOK_RPAREN;
    t.len = 1;
    return t;
  }
  if (c == ';') {
    lx->i++;
    t.kind = TOK_SEMI;
    t.len = 1;
    return t;
  }

  if (c == '"') {
    size_t start = lx->i;
    lx->i++; // opening quote

    while (lx->i < lx->len) {
      char ch = lx->src[lx->i++];
      if (ch == '"') {
        t.kind = TOK_STRING;
        t.pos = start;
        t.len = lx->i - start;
        return t;
      }
      if (ch == '\\') {
        if (lx->i >= lx->len) break;
        char esc = lx->src[lx->i++];
        switch (esc) {
          case 'n': bb_push(&t.str_bytes, (uint8_t)'\n'); break;
          case 't': bb_push(&t.str_bytes, (uint8_t)'\t'); break;
          case '\\': bb_push(&t.str_bytes, (uint8_t)'\\'); break;
          case '"': bb_push(&t.str_bytes, (uint8_t)'"'); break;
          default:
            // unknown escape: keep it literal (v0 behavior)
            bb_push(&t.str_bytes, (uint8_t)esc);
            break;
        }
      } else {
        bb_push(&t.str_bytes, (uint8_t)ch);
      }
    }

    fprintf(stderr, "zmc: unterminated string literal\n");
    exit(2);
  }

  fprintf(stderr, "zmc: unexpected character '%c' at byte %zu\n", c, lx->i);
  exit(2);
}

// ------------------------------ IR (minimal) ------------------------------

typedef struct {
  char* label;
  ByteBuf bytes; // string payload bytes
} StrLit;

typedef struct {
  StrLit* items;
  size_t len;
  size_t cap;
} StrPool;

static void sp_init(StrPool* sp) {
  sp->items = NULL;
  sp->len = 0;
  sp->cap = 0;
}

static void sp_free(StrPool* sp) {
  for (size_t i = 0; i < sp->len; i++) {
    free(sp->items[i].label);
    bb_free(&sp->items[i].bytes);
  }
  free(sp->items);
  sp->items = NULL;
  sp->len = 0;
  sp->cap = 0;
}

static const char* sp_add(StrPool* sp, const ByteBuf* bytes) {
  if (sp->len == sp->cap) {
    size_t new_cap = sp->cap ? (sp->cap * 2) : 16;
    sp->items = (StrLit*)xrealloc(sp->items, new_cap * sizeof(StrLit));
    sp->cap = new_cap;
  }

  size_t id = sp->len++;
  StrLit* it = &sp->items[id];
  memset(it, 0, sizeof(*it));

  char tmp[64];
  snprintf(tmp, sizeof(tmp), "str_lit_%zu", id);
  it->label = (char*)xmalloc(strlen(tmp) + 1);
  strcpy(it->label, tmp);

  bb_init(&it->bytes);
  for (size_t i = 0; i < bytes->len; i++) bb_push(&it->bytes, bytes->data[i]);

  return it->label;
}

// ------------------------------ Parser (minimal) ------------------------------

typedef struct {
  Lexer lx;
  Token cur;
  const char* src;
} Parser;

static void parse_init(Parser* p, const char* src, size_t len) {
  p->src = src;
  lex_init(&p->lx, src, len);
  p->cur = next_token(&p->lx);
}

static void advance(Parser* p) {
  token_free(&p->cur);
  p->cur = next_token(&p->lx);
}

static bool tok_is(Parser* p, TokKind k) { return p->cur.kind == k; }

static void expect(Parser* p, TokKind k, const char* what) {
  if (!tok_is(p, k)) {
    fprintf(stderr, "zmc: expected %s\n", what);
    exit(2);
  }
}

typedef struct {
  const char** print_labels;
  size_t print_len;
  size_t print_cap;
} Program;

static void prog_init(Program* pr) {
  pr->print_labels = NULL;
  pr->print_len = 0;
  pr->print_cap = 0;
}

static void prog_free(Program* pr) {
  free(pr->print_labels);
  pr->print_labels = NULL;
  pr->print_len = 0;
  pr->print_cap = 0;
}

static void prog_add_print(Program* pr, const char* label) {
  if (pr->print_len == pr->print_cap) {
    size_t new_cap = pr->print_cap ? (pr->print_cap * 2) : 16;
    pr->print_labels = (const char**)xrealloc(pr->print_labels, new_cap * sizeof(char*));
    pr->print_cap = new_cap;
  }
  pr->print_labels[pr->print_len++] = label;
}

static bool ident_is(Parser* p, const char* s) {
  size_t n = strlen(s);
  if (!tok_is(p, TOK_IDENT) || p->cur.len != n) return false;
  return memcmp(p->src + p->cur.pos, s, n) == 0;
}

static void parse_program(Parser* p, StrPool* sp, Program* out) {
  while (!tok_is(p, TOK_EOF)) {
    // stmt := print("...");
    if (!ident_is(p, "print")) {
      fprintf(stderr, "zmc: only print(\"...\"); is supported in v0 (at byte %zu)\n", p->cur.pos);
      exit(2);
    }
    advance(p);

    expect(p, TOK_LPAREN, "'('");
    advance(p);

    expect(p, TOK_STRING, "string literal");
    const char* label = sp_add(sp, &p->cur.str_bytes);
    advance(p);

    expect(p, TOK_RPAREN, "')'");
    advance(p);

    expect(p, TOK_SEMI, "';'");
    advance(p);

    prog_add_print(out, label);
  }
}

// ------------------------------ Assembly emission ------------------------------

static void emit_bytes_as_byte_directives(FILE* out, const uint8_t* bytes, size_t n) {
  const size_t per_line = 16;
  for (size_t i = 0; i < n; i += per_line) {
    fprintf(out, "  .byte ");
    size_t end = i + per_line;
    if (end > n) end = n;
    for (size_t j = i; j < end; j++) {
      fprintf(out, "%u", (unsigned)bytes[j]);
      if (j + 1 < end) fprintf(out, ", ");
    }
    fprintf(out, "\n");
  }
}

static bool emit_bytes_as_ascii(FILE* out, const uint8_t* bytes, size_t n) {
  // Try to emit `.ascii "..."` using escapes supported by svm_asm.
  // IMPORTANT: the current svm_asm implementation tokenizes strings into NUL-terminated C strings,
  // so embedded NUL bytes cannot be represented safely via `.ascii`.
  for (size_t i = 0; i < n; i++) {
    if (bytes[i] == 0) return false;
  }

  // svm_asm has an internal ~4096 token buffer for strings. Keep some margin.
  // If we would exceed it due to escaping, fall back to .byte.
  size_t worst_case = 2; // quotes
  for (size_t i = 0; i < n; i++) {
    uint8_t b = bytes[i];
    if (b == '\\' || b == '"') worst_case += 2;
    else if (b == '\n' || b == '\t' || b == '\r') worst_case += 2;
    else if (b < 32 || b >= 127) worst_case += 4; // \xHH
    else worst_case += 1;
  }
  if (worst_case >= 4000) return false;

  fprintf(out, "  .ascii \"");
  for (size_t i = 0; i < n; i++) {
    uint8_t b = bytes[i];
    switch (b) {
      case '\\': fputs("\\\\", out); break;
      case '"':  fputs("\\\"", out); break;
      case '\n': fputs("\\n", out); break;
      case '\t': fputs("\\t", out); break;
      case '\r': fputs("\\r", out); break;
      default:
        if (b < 32 || b >= 127) {
          fprintf(out, "\\x%02X", (unsigned)b);
        } else {
          fputc((int)b, out);
        }
        break;
    }
  }
  fprintf(out, "\"\n");
  return true;
}

static void emit_program_asm(FILE* out, const Program* pr, const StrPool* sp) {
  fprintf(out, ".module \"zman_program\"\n\n");
  fprintf(out, ".code\n");
  fprintf(out, ".entry main\n\n");

  // Helper: __zman_print(p) -> 0
  // StackVM arg layout after CALL: p is at [fp-3]
  fprintf(out, "__zman_print:\n");
  fprintf(out, "  ENTER 0\n");
  fprintf(out, "  LDFP -3\n");
  fprintf(out, "  DUP\n");
  fprintf(out, "  LOAD32\n");
  fprintf(out, "  SWAP\n");
  fprintf(out, "  ADDI 4\n");
  fprintf(out, "  SWAP\n");
  fprintf(out, "  SYSCALL 4\n");
  fprintf(out, "  PUSHI 0\n");
  fprintf(out, "  RET 1\n\n");

  fprintf(out, "main:\n");
  for (size_t i = 0; i < pr->print_len; i++) {
    fprintf(out, "  PUSHI %s\n", pr->print_labels[i]);
    fprintf(out, "  CALL __zman_print\n");
    fprintf(out, "  POP\n");
  }
  fprintf(out, "  HALT\n\n");

  fprintf(out, ".data\n");
  fprintf(out, "  .align 4\n\n");

  for (size_t i = 0; i < sp->len; i++) {
    const StrLit* s = &sp->items[i];
    fprintf(out, "%s:\n", s->label);
    fprintf(out, "  .word %zu\n", s->bytes.len);
    if (!emit_bytes_as_ascii(out, s->bytes.data, s->bytes.len)) {
      emit_bytes_as_byte_directives(out, s->bytes.data, s->bytes.len);
    }
    fprintf(out, "  .align 4\n\n");
  }

  fprintf(out, ".end\n");
}

// ------------------------------ CLI ------------------------------

static void usage(FILE* out) {
  fprintf(out,
          "Usage: zmc <input.zm> <output.asm>\n"
          "\n"
          "Minimal ZManLang compiler (v0).\n"
          "Currently supported subset:\n"
          "  - print(\"...\");  (string literal only)\n");
}

int main(int argc, char** argv) {
  if (argc != 3) {
    usage(stderr);
    return 2;
  }

  const char* in_path = argv[1];
  const char* out_path = argv[2];

  size_t src_len = 0;
  char* src = read_entire_file(in_path, &src_len);

  Parser p;
  parse_init(&p, src, src_len);

  StrPool sp;
  sp_init(&sp);

  Program pr;
  prog_init(&pr);

  parse_program(&p, &sp, &pr);
  token_free(&p.cur);

  FILE* out = fopen(out_path, "wb");
  if (!out) {
    fprintf(stderr, "zmc: failed to open '%s' for writing: %s\n", out_path, strerror(errno));
    return 2;
  }

  emit_program_asm(out, &pr, &sp);
  fclose(out);

  prog_free(&pr);
  sp_free(&sp);
  free(src);

  return 0;
}
