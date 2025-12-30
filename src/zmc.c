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

static void bb_free(ByteBuf* b) {
  free(b->data);
  b->data = NULL;
  b->len = 0;
  b->cap = 0;
}

static void bb_reserve(ByteBuf* b, size_t need) {
  if (need <= b->cap) return;
  size_t new_cap = b->cap ? b->cap : 16;
  while (new_cap < need) new_cap *= 2;
  b->data = (uint8_t*)xrealloc(b->data, new_cap);
  b->cap = new_cap;
}

static void bb_push(ByteBuf* b, uint8_t v) {
  if (b->len + 1 > b->cap) bb_reserve(b, b->len + 1);
  b->data[b->len++] = v;
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
  TOK_LET,
  TOK_CONST,
  TOK_FUNC,
  TOK_RETURN,
  TOK_IF,
  TOK_ELSE,
  TOK_WHILE,
  TOK_TRUE,
  TOK_FALSE,
  TOK_AND,
  TOK_OR,
  TOK_NOT,
  TOK_ASSIGN, // :=
  TOK_PLUS,
  TOK_MINUS,
  TOK_STAR,
  TOK_SLASH,
  TOK_PERCENT,
  TOK_BANG, // !
  TOK_EQ,   // =
  TOK_NE,   // !=
  TOK_LT,   // <
  TOK_GT,   // >
  TOK_LE,   // <=
  TOK_GE,   // >=
  TOK_LPAREN,
  TOK_RPAREN,
  TOK_LBRACE,
  TOK_RBRACE,
  TOK_COMMA,
  TOK_SEMI,
  TOK_STRING,
  TOK_INT,
} TokKind;

typedef struct {
  TokKind kind;
  size_t pos;
  size_t len;
  // for TOK_STRING
  ByteBuf str_bytes;
  // for TOK_INT
  uint32_t int_u32;
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

  if (c >= '0' && c <= '9') {
    size_t start = lx->i;
    uint64_t v = 0;
    while (lx->i < lx->len) {
      char d = lx->src[lx->i];
      if (d < '0' || d > '9') break;
      v = v * 10u + (uint64_t)(d - '0');
      if (v > 0xFFFFFFFFu) {
        fprintf(stderr, "zmc: integer literal too large at byte %zu\n", start);
        exit(2);
      }
      lx->i++;
    }
    t.kind = TOK_INT;
    t.pos = start;
    t.len = lx->i - start;
    t.int_u32 = (uint32_t)v;
    return t;
  }

  if (is_alpha_(c)) {
    size_t start = lx->i;
    lx->i++;
    while (lx->i < lx->len && is_alnum_(lx->src[lx->i])) lx->i++;
    t.kind = TOK_IDENT;
    t.pos = start;
    t.len = lx->i - start;

    // keywords (v0 subset)
    if (t.len == 3 && memcmp(lx->src + t.pos, "let", 3) == 0) t.kind = TOK_LET;
    else if (t.len == 5 && memcmp(lx->src + t.pos, "const", 5) == 0) t.kind = TOK_CONST;
    else if (t.len == 4 && memcmp(lx->src + t.pos, "func", 4) == 0) t.kind = TOK_FUNC;
    else if (t.len == 6 && memcmp(lx->src + t.pos, "return", 6) == 0) t.kind = TOK_RETURN;
    else if (t.len == 2 && memcmp(lx->src + t.pos, "if", 2) == 0) t.kind = TOK_IF;
    else if (t.len == 4 && memcmp(lx->src + t.pos, "else", 4) == 0) t.kind = TOK_ELSE;
    else if (t.len == 5 && memcmp(lx->src + t.pos, "while", 5) == 0) t.kind = TOK_WHILE;
    else if (t.len == 4 && memcmp(lx->src + t.pos, "true", 4) == 0) t.kind = TOK_TRUE;
    else if (t.len == 5 && memcmp(lx->src + t.pos, "false", 5) == 0) t.kind = TOK_FALSE;
    else if (t.len == 3 && memcmp(lx->src + t.pos, "and", 3) == 0) t.kind = TOK_AND;
    else if (t.len == 2 && memcmp(lx->src + t.pos, "or", 2) == 0) t.kind = TOK_OR;
    else if (t.len == 3 && memcmp(lx->src + t.pos, "not", 3) == 0) t.kind = TOK_NOT;

    return t;
  }

  if (c == ':' && lx->i + 1 < lx->len && lx->src[lx->i + 1] == '=') {
    lx->i += 2;
    t.kind = TOK_ASSIGN;
    t.len = 2;
    return t;
  }

  if (c == '!') {
    if (lx->i + 1 < lx->len && lx->src[lx->i + 1] == '=') {
      lx->i += 2;
      t.kind = TOK_NE;
      t.len = 2;
      return t;
    }
    lx->i++;
    t.kind = TOK_BANG;
    t.len = 1;
    return t;
  }

  if (c == '<') {
    size_t start = lx->i;
    lx->i++;
    if (lx->i < lx->len && lx->src[lx->i] == '=') {
      lx->i++;
      t.kind = TOK_LE;
      t.len = 2;
      t.pos = start;
      return t;
    }
    t.kind = TOK_LT;
    t.len = 1;
    t.pos = start;
    return t;
  }

  if (c == '>') {
    size_t start = lx->i;
    lx->i++;
    if (lx->i < lx->len && lx->src[lx->i] == '=') {
      lx->i++;
      t.kind = TOK_GE;
      t.len = 2;
      t.pos = start;
      return t;
    }
    t.kind = TOK_GT;
    t.len = 1;
    t.pos = start;
    return t;
  }

  if (c == '=') {
    lx->i++;
    t.kind = TOK_EQ;
    t.len = 1;
    return t;
  }

  if (c == '+') {
    lx->i++;
    t.kind = TOK_PLUS;
    t.len = 1;
    return t;
  }

  if (c == '-') {
    lx->i++;
    t.kind = TOK_MINUS;
    t.len = 1;
    return t;
  }

  if (c == '*') {
    lx->i++;
    t.kind = TOK_STAR;
    t.len = 1;
    return t;
  }

  if (c == '/') {
    lx->i++;
    t.kind = TOK_SLASH;
    t.len = 1;
    return t;
  }

  if (c == '%') {
    lx->i++;
    t.kind = TOK_PERCENT;
    t.len = 1;
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
  if (c == '{') {
    lx->i++;
    t.kind = TOK_LBRACE;
    t.len = 1;
    return t;
  }
  if (c == '}') {
    lx->i++;
    t.kind = TOK_RBRACE;
    t.len = 1;
    return t;
  }
  if (c == ';') {
    lx->i++;
    t.kind = TOK_SEMI;
    t.len = 1;
    return t;
  }

  if (c == ',') {
    lx->i++;
    t.kind = TOK_COMMA;
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
  char* name;
  char* data_label;
  bool is_const;
  int ty; // Type
} Global;

typedef struct {
  Global* items;
  size_t len;
  size_t cap;
} Globals;

static void globals_init(Globals* g) {
  g->items = NULL;
  g->len = 0;
  g->cap = 0;
}

static void globals_free(Globals* g) {
  for (size_t i = 0; i < g->len; i++) {
    free(g->items[i].name);
    free(g->items[i].data_label);
  }
  free(g->items);
  g->items = NULL;
  g->len = 0;
  g->cap = 0;
}

static const Global* globals_find(const Globals* g, const char* name, size_t name_len) {
  for (size_t i = 0; i < g->len; i++) {
    if (strlen(g->items[i].name) == name_len && memcmp(g->items[i].name, name, name_len) == 0) return &g->items[i];
  }
  return NULL;
}

static Global* globals_add(Globals* g, const char* name, size_t name_len, bool is_const) {
  if (globals_find(g, name, name_len)) {
    fprintf(stderr, "zmc: duplicate global '%.*s'\n", (int)name_len, name);
    exit(2);
  }

  if (g->len == g->cap) {
    size_t new_cap = g->cap ? (g->cap * 2) : 16;
    g->items = (Global*)xrealloc(g->items, new_cap * sizeof(Global));
    g->cap = new_cap;
  }

  size_t id = g->len++;
  Global* it = &g->items[id];
  memset(it, 0, sizeof(*it));

  it->name = (char*)xmalloc(name_len + 1);
  memcpy(it->name, name, name_len);
  it->name[name_len] = 0;

  char tmp[128];
  snprintf(tmp, sizeof(tmp), "g_%s", it->name);
  it->data_label = (char*)xmalloc(strlen(tmp) + 1);
  strcpy(it->data_label, tmp);

  it->is_const = is_const;
  return it;
}

typedef enum {
  TY_STRING = 1,
  TY_I32 = 2,
  TY_BOOL = 3,
} Type;

typedef enum {
  REF_GLOBAL = 0,
  REF_LOCAL = 1,
  REF_PARAM = 2,
} RefKind;

static const char* type_name(Type t) {
  switch (t) {
    case TY_STRING: return "string";
    case TY_I32: return "i32";
    case TY_BOOL: return "bool";
    default: return "<unknown>";
  }
}

static void globals_set_type(Global* g, Type ty) {
  g->ty = (int)ty;
}

typedef enum {
  EXPR_STR_LIT,
  EXPR_INT_LIT,
  EXPR_BOOL_LIT,
  EXPR_IDENT,
  EXPR_CALL,
  EXPR_ADD,
  EXPR_SUB,
  EXPR_MUL,
  EXPR_DIVS,
  EXPR_MODS,
  EXPR_NEG,
  EXPR_LNOT,
  EXPR_TEXT,
  EXPR_NUMBER,
  EXPR_EQ,
  EXPR_NE,
  EXPR_LT,
  EXPR_GT,
  EXPR_LE,
  EXPR_GE,
  EXPR_AND,
  EXPR_OR,
} ExprKind;

typedef struct Function Function;

typedef struct Expr Expr;
struct Expr {
  ExprKind kind;
  size_t pos;
  Type ty;
  union {
    const char* str_label; // EXPR_STR_LIT
    uint32_t int_u32; // EXPR_INT_LIT
    struct {
      const char* name;
      size_t name_len;
      RefKind ref;
      int slot; // for locals/params: fp+slot (slot may be negative for params)
      const char* data_label; // for globals
      size_t param_index; // for params
    } ident; // EXPR_IDENT
    struct {
      const char* name;
      size_t name_len;
      Expr** args;
      size_t argc;
      Function* fn; // resolved callee
    } call; // EXPR_CALL
    struct { Expr* left; Expr* right; } bin; // EXPR_ADD/EXPR_SUB/EXPR_MUL/EXPR_DIVS/EXPR_MODS
    struct { Expr* inner; } unary; // EXPR_NEG
  } v;
};

static Expr* new_expr(ExprKind k, size_t pos) {
  Expr* e = (Expr*)xmalloc(sizeof(Expr));
  memset(e, 0, sizeof(*e));
  e->kind = k;
  e->pos = pos;
  e->ty = 0;
  return e;
}

static void free_expr(Expr* e) {
  if (!e) return;
  switch (e->kind) {
    case EXPR_CALL:
      for (size_t i = 0; i < e->v.call.argc; i++) free_expr(e->v.call.args[i]);
      free(e->v.call.args);
      break;
    case EXPR_ADD:
    case EXPR_SUB:
    case EXPR_MUL:
    case EXPR_DIVS:
    case EXPR_MODS:
    case EXPR_EQ:
    case EXPR_NE:
    case EXPR_LT:
    case EXPR_GT:
    case EXPR_LE:
    case EXPR_GE:
    case EXPR_AND:
    case EXPR_OR:
      free_expr(e->v.bin.left);
      free_expr(e->v.bin.right);
      break;
    case EXPR_NEG:
    case EXPR_LNOT:
      free_expr(e->v.unary.inner);
      break;
    case EXPR_TEXT:
    case EXPR_NUMBER:
      free_expr(e->v.unary.inner);
      break;
    default:
      break;
  }
  free(e);
}

typedef enum {
  STMT_LET,
  STMT_CONST,
  STMT_ASSIGN,
  STMT_PRINT,
  STMT_RETURN,
  STMT_EXPR,
  STMT_IF,
  STMT_WHILE,
  STMT_BLOCK,
} StmtKind;

typedef struct StmtList StmtList;

typedef struct {
  StmtKind kind;
  size_t pos;
  union {
    struct {
      const char* name;
      size_t name_len;
      Expr* value;
      RefKind ref;
      int slot;
      const char* data_label;
      size_t param_index;
    } bind; // let/const (ref is GLOBAL for top-level; LOCAL inside functions)
    struct {
      const char* name;
      size_t name_len;
      Expr* value;
      RefKind ref;
      int slot;
      const char* data_label;
      size_t param_index;
      bool target_is_const;
      Type target_type;
    } assign;
    struct { Expr* value; } print;
    struct { Expr* value; } ret;
    struct { Expr* value; } expr;
    struct { Expr* cond; StmtList* then_body; StmtList* else_body; } if_; // else_body may be NULL
    struct { Expr* cond; StmtList* body; } while_;
    struct { StmtList* body; } block;
  } v;
} Stmt;

struct StmtList {
  Stmt* items;
  size_t len;
  size_t cap;
};

static StmtList* stmt_list_new(void) {
  StmtList* s = (StmtList*)xmalloc(sizeof(StmtList));
  s->items = NULL;
  s->len = 0;
  s->cap = 0;
  return s;
}

static void stmt_free(Stmt* st);

static void stmt_list_free(StmtList* s) {
  if (!s) return;
  for (size_t i = 0; i < s->len; i++) stmt_free(&s->items[i]);
  free(s->items);
  free(s);
}

static void stmt_free(Stmt* st) {
  switch (st->kind) {
    case STMT_LET:
    case STMT_CONST:
      free_expr(st->v.bind.value);
      return;
    case STMT_ASSIGN:
      free_expr(st->v.assign.value);
      return;
    case STMT_PRINT:
      free_expr(st->v.print.value);
      return;
    case STMT_RETURN:
      free_expr(st->v.ret.value);
      return;
    case STMT_EXPR:
      free_expr(st->v.expr.value);
      return;
    case STMT_IF:
      free_expr(st->v.if_.cond);
      stmt_list_free(st->v.if_.then_body);
      stmt_list_free(st->v.if_.else_body);
      return;
    case STMT_WHILE:
      free_expr(st->v.while_.cond);
      stmt_list_free(st->v.while_.body);
      return;
    case STMT_BLOCK:
      stmt_list_free(st->v.block.body);
      return;
  }
}

static void stmt_list_push(StmtList* s, Stmt st) {
  if (s->len == s->cap) {
    size_t new_cap = s->cap ? (s->cap * 2) : 32;
    s->items = (Stmt*)xrealloc(s->items, new_cap * sizeof(Stmt));
    s->cap = new_cap;
  }
  s->items[s->len++] = st;
}

typedef struct {
  Lexer lx;
  Token cur;
  const char* src;
} Parser;

typedef struct {
  char* name;
  bool is_const;
  Type ty; // 0 until inferred
  int slot; // fp + slot (>=0)
} Local;

typedef struct {
  Local* items;
  size_t len;
  size_t cap;
} Locals;

static void locals_init(Locals* l) {
  l->items = NULL;
  l->len = 0;
  l->cap = 0;
}

static void locals_free(Locals* l) {
  for (size_t i = 0; i < l->len; i++) free(l->items[i].name);
  free(l->items);
  l->items = NULL;
  l->len = 0;
  l->cap = 0;
}

static Local* locals_find(Locals* l, const char* name, size_t name_len) {
  for (size_t i = 0; i < l->len; i++) {
    if (strlen(l->items[i].name) == name_len && memcmp(l->items[i].name, name, name_len) == 0) return &l->items[i];
  }
  return NULL;
}

static Local* locals_add(Locals* l, const char* name, size_t name_len, bool is_const, int slot) {
  if (locals_find(l, name, name_len)) {
    fprintf(stderr, "zmc: duplicate local '%.*s'\n", (int)name_len, name);
    exit(2);
  }
  if (l->len == l->cap) {
    size_t new_cap = l->cap ? (l->cap * 2) : 16;
    l->items = (Local*)xrealloc(l->items, new_cap * sizeof(Local));
    l->cap = new_cap;
  }
  Local* it = &l->items[l->len++];
  memset(it, 0, sizeof(*it));
  it->name = (char*)xmalloc(name_len + 1);
  memcpy(it->name, name, name_len);
  it->name[name_len] = 0;
  it->is_const = is_const;
  it->ty = 0;
  it->slot = slot;
  return it;
}

struct Function {
  char* name;
  char* label;
  size_t argc;
  Type* param_types; // length argc
  char** param_names; // length argc
  Type ret_type; // 0 until inferred
  // locals (fp+0..)
  Locals locals;
  int nlocals;
  // body
  StmtList* body;
};

typedef struct {
  Function* items;
  size_t len;
  size_t cap;
} FuncTable;

static void ft_init(FuncTable* ft) {
  ft->items = NULL;
  ft->len = 0;
  ft->cap = 0;
}

static void ft_free(FuncTable* ft) {
  for (size_t i = 0; i < ft->len; i++) {
    Function* f = &ft->items[i];
    free(f->name);
    free(f->label);
    free(f->param_types);
    if (f->param_names) {
      for (size_t j = 0; j < f->argc; j++) free(f->param_names[j]);
      free(f->param_names);
    }
    locals_free(&f->locals);
    stmt_list_free(f->body);
  }
  free(ft->items);
  ft->items = NULL;
  ft->len = 0;
  ft->cap = 0;
}

static Function* ft_find(FuncTable* ft, const char* name, size_t name_len) {
  for (size_t i = 0; i < ft->len; i++) {
    if (strlen(ft->items[i].name) == name_len && memcmp(ft->items[i].name, name, name_len) == 0) return &ft->items[i];
  }
  return NULL;
}

static Function* ft_add(FuncTable* ft, const char* name, size_t name_len, size_t argc) {
  if (ft_find(ft, name, name_len)) {
    fprintf(stderr, "zmc: duplicate function '%.*s'\n", (int)name_len, name);
    exit(2);
  }
  if (ft->len == ft->cap) {
    size_t new_cap = ft->cap ? (ft->cap * 2) : 16;
    ft->items = (Function*)xrealloc(ft->items, new_cap * sizeof(Function));
    ft->cap = new_cap;
  }
  size_t id = ft->len++;
  Function* f = &ft->items[id];
  memset(f, 0, sizeof(*f));
  f->name = (char*)xmalloc(name_len + 1);
  memcpy(f->name, name, name_len);
  f->name[name_len] = 0;
  char lbl[128];
  snprintf(lbl, sizeof(lbl), "fn_%s", f->name);
  f->label = (char*)xmalloc(strlen(lbl) + 1);
  strcpy(f->label, lbl);
  f->argc = argc;
  f->param_types = (Type*)xmalloc(argc * sizeof(Type));
  for (size_t i = 0; i < argc; i++) f->param_types[i] = 0;
  f->param_names = (char**)xmalloc(argc * sizeof(char*));
  for (size_t i = 0; i < argc; i++) f->param_names[i] = NULL;
  f->ret_type = 0;
  locals_init(&f->locals);
  f->nlocals = 0;
  f->body = NULL;
  return f;
}

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

static bool ident_is(Parser* p, const char* s) {
  size_t n = strlen(s);
  if (!tok_is(p, TOK_IDENT) || p->cur.len != n) return false;
  return memcmp(p->src + p->cur.pos, s, n) == 0;
}

typedef struct {
  Globals* globals;
  FuncTable* funcs;
  Function* cur_fn; // NULL at top-level
} ParseCtx;

static int fn_param_index(Function* f, const char* name, size_t name_len) {
  for (size_t i = 0; i < f->argc; i++) {
    if (strlen(f->param_names[i]) == name_len && memcmp(f->param_names[i], name, name_len) == 0) return (int)i;
  }
  return -1;
}

static void set_expr_type(ParseCtx* ctx, Expr* e, Type ty);

static void require_expr_type(ParseCtx* ctx, Expr* e, Type want, const char* where) {
  if (e->ty == 0) {
    set_expr_type(ctx, e, want);
    return;
  }
  if (e->ty != want) {
    fprintf(stderr, "zmc: type error (%s): expected %s, got %s\n", where, type_name(want), type_name(e->ty));
    exit(2);
  }
}

static Expr* parse_expr(Parser* p, StrPool* sp, ParseCtx* ctx);
static StmtList* parse_block(Parser* p, StrPool* sp, ParseCtx* ctx);

static void resolve_ident_expr(ParseCtx* ctx, Expr* e) {
  if (!ctx->cur_fn) {
    const Global* g = globals_find(ctx->globals, e->v.ident.name, e->v.ident.name_len);
    if (!g || g->ty == 0) {
      fprintf(stderr, "zmc: undefined identifier '%.*s'\n", (int)e->v.ident.name_len, e->v.ident.name);
      exit(2);
    }
    e->v.ident.ref = REF_GLOBAL;
    e->v.ident.data_label = g->data_label;
    e->ty = (Type)g->ty;
    return;
  }

  Local* l = locals_find(&ctx->cur_fn->locals, e->v.ident.name, e->v.ident.name_len);
  if (l) {
    e->v.ident.ref = REF_LOCAL;
    e->v.ident.slot = l->slot;
    e->ty = l->ty;
    return;
  }

  int pi = fn_param_index(ctx->cur_fn, e->v.ident.name, e->v.ident.name_len);
  if (pi >= 0) {
    e->v.ident.ref = REF_PARAM;
    e->v.ident.param_index = (size_t)pi;
    e->v.ident.slot = -(int)(2 + ctx->cur_fn->argc - (size_t)pi);
    e->ty = ctx->cur_fn->param_types[pi];
    return;
  }

  const Global* g = globals_find(ctx->globals, e->v.ident.name, e->v.ident.name_len);
  if (!g || g->ty == 0) {
    fprintf(stderr, "zmc: undefined identifier '%.*s'\n", (int)e->v.ident.name_len, e->v.ident.name);
    exit(2);
  }
  e->v.ident.ref = REF_GLOBAL;
  e->v.ident.data_label = g->data_label;
  e->ty = (Type)g->ty;
}

static void set_expr_type(ParseCtx* ctx, Expr* e, Type ty) {
  if (ty == 0) return;
  if (e->ty == 0) {
    e->ty = ty;
  } else if (e->ty != ty) {
    fprintf(stderr, "zmc: type error: expected %s, got %s\n", type_name(ty), type_name(e->ty));
    exit(2);
  }

  if (ctx->cur_fn && e->kind == EXPR_IDENT) {
    if (e->v.ident.ref == REF_LOCAL) {
      Local* l = locals_find(&ctx->cur_fn->locals, e->v.ident.name, e->v.ident.name_len);
      if (l && l->ty == 0) l->ty = ty;
    } else if (e->v.ident.ref == REF_PARAM) {
      if (ctx->cur_fn->param_types[e->v.ident.param_index] == 0) ctx->cur_fn->param_types[e->v.ident.param_index] = ty;
    }
  }

  if (e->kind == EXPR_CALL && e->v.call.fn) {
    if (e->v.call.fn->ret_type == 0) e->v.call.fn->ret_type = ty;
  }
}

static Expr* parse_primary(Parser* p, StrPool* sp, ParseCtx* ctx) {
  if (tok_is(p, TOK_TRUE)) {
    Expr* e = new_expr(EXPR_BOOL_LIT, p->cur.pos);
    e->v.int_u32 = 1;
    e->ty = TY_BOOL;
    advance(p);
    return e;
  }
  if (tok_is(p, TOK_FALSE)) {
    Expr* e = new_expr(EXPR_BOOL_LIT, p->cur.pos);
    e->v.int_u32 = 0;
    e->ty = TY_BOOL;
    advance(p);
    return e;
  }
  if (ident_is(p, "text")) {
    size_t pos = p->cur.pos;
    advance(p);
    expect(p, TOK_LPAREN, "'('");
    advance(p);
    Expr* inner = parse_expr(p, sp, ctx);
    require_expr_type(ctx, inner, TY_I32, "text() argument");
    expect(p, TOK_RPAREN, "')'");
    advance(p);
    Expr* e = new_expr(EXPR_TEXT, pos);
    e->v.unary.inner = inner;
    e->ty = TY_STRING;
    return e;
  }

  if (ident_is(p, "number")) {
    size_t pos = p->cur.pos;
    advance(p);
    expect(p, TOK_LPAREN, "'('");
    advance(p);
    Expr* inner = parse_expr(p, sp, ctx);
    require_expr_type(ctx, inner, TY_STRING, "number() argument");
    expect(p, TOK_RPAREN, "')'");
    advance(p);
    Expr* e = new_expr(EXPR_NUMBER, pos);
    e->v.unary.inner = inner;
    e->ty = TY_I32;
    return e;
  }

  if (tok_is(p, TOK_STRING)) {
    Expr* e = new_expr(EXPR_STR_LIT, p->cur.pos);
    e->v.str_label = sp_add(sp, &p->cur.str_bytes);
    e->ty = TY_STRING;
    advance(p);
    return e;
  }
  if (tok_is(p, TOK_INT)) {
    Expr* e = new_expr(EXPR_INT_LIT, p->cur.pos);
    e->v.int_u32 = p->cur.int_u32;
    e->ty = TY_I32;
    advance(p);
    return e;
  }
  if (tok_is(p, TOK_IDENT)) {
    size_t pos = p->cur.pos;
    const char* name = p->src + p->cur.pos;
    size_t name_len = p->cur.len;
    advance(p);

    if (tok_is(p, TOK_LPAREN)) {
      advance(p);
      Expr** args = NULL;
      size_t argc = 0;
      size_t cap = 0;
      if (!tok_is(p, TOK_RPAREN)) {
        for (;;) {
          if (argc == cap) {
            size_t new_cap = cap ? (cap * 2) : 4;
            args = (Expr**)xrealloc(args, new_cap * sizeof(Expr*));
            cap = new_cap;
          }
          args[argc++] = parse_expr(p, sp, ctx);
          if (tok_is(p, TOK_COMMA)) {
            advance(p);
            continue;
          }
          break;
        }
      }
      expect(p, TOK_RPAREN, "')'");
      advance(p);

      Function* fn = ft_find(ctx->funcs, name, name_len);
      if (!fn) {
        fprintf(stderr, "zmc: call to undefined function '%.*s'\n", (int)name_len, name);
        exit(2);
      }
      if (fn->argc != argc) {
        fprintf(stderr, "zmc: wrong argument count calling '%s': expected %zu, got %zu\n", fn->name, fn->argc, argc);
        exit(2);
      }
      for (size_t i = 0; i < argc; i++) {
        if (fn->param_types[i] != 0) {
          require_expr_type(ctx, args[i], fn->param_types[i], "call argument");
        } else if (args[i]->ty != 0) {
          fn->param_types[i] = args[i]->ty;
        }
      }

      Expr* e = new_expr(EXPR_CALL, pos);
      e->v.call.name = name;
      e->v.call.name_len = name_len;
      e->v.call.args = args;
      e->v.call.argc = argc;
      e->v.call.fn = fn;
      e->ty = fn->ret_type;
      return e;
    }

    Expr* e = new_expr(EXPR_IDENT, pos);
    e->v.ident.name = name;
    e->v.ident.name_len = name_len;
    e->v.ident.ref = REF_GLOBAL;
    e->v.ident.slot = 0;
    e->v.ident.data_label = NULL;
    e->v.ident.param_index = 0;
    resolve_ident_expr(ctx, e);
    return e;
  }
  if (tok_is(p, TOK_LPAREN)) {
    advance(p);
    Expr* e = parse_expr(p, sp, ctx);
    expect(p, TOK_RPAREN, "')'");
    advance(p);
    return e;
  }

  fprintf(stderr, "zmc: expected expression at byte %zu\n", p->cur.pos);
  exit(2);
}

static Expr* parse_unary(Parser* p, StrPool* sp, ParseCtx* ctx) {
  if (tok_is(p, TOK_BANG) || tok_is(p, TOK_NOT)) {
    size_t pos = p->cur.pos;
    advance(p);
    Expr* inner = parse_unary(p, sp, ctx);
    if (!(inner->ty == TY_BOOL || inner->ty == TY_I32 || inner->ty == 0)) {
      fprintf(stderr, "zmc: type error (!/not): expected bool or i32, got %s\n", type_name(inner->ty));
      exit(2);
    }
    if (inner->ty == 0) set_expr_type(ctx, inner, TY_I32);
    Expr* e = new_expr(EXPR_LNOT, pos);
    e->v.unary.inner = inner;
    e->ty = TY_BOOL;
    return e;
  }
  if (tok_is(p, TOK_MINUS)) {
    size_t pos = p->cur.pos;
    advance(p);
    Expr* inner = parse_unary(p, sp, ctx);
    require_expr_type(ctx, inner, TY_I32, "unary -");
    Expr* e = new_expr(EXPR_NEG, pos);
    e->v.unary.inner = inner;
    e->ty = TY_I32;
    return e;
  }
  return parse_primary(p, sp, ctx);
}

static Expr* parse_mul(Parser* p, StrPool* sp, ParseCtx* ctx) {
  Expr* left = parse_unary(p, sp, ctx);
  while (tok_is(p, TOK_STAR) || tok_is(p, TOK_SLASH) || tok_is(p, TOK_PERCENT)) {
    TokKind op = p->cur.kind;
    size_t pos = p->cur.pos;
    advance(p);
    Expr* right = parse_unary(p, sp, ctx);
    require_expr_type(ctx, left, TY_I32, "mul/div/mod left");
    require_expr_type(ctx, right, TY_I32, "mul/div/mod right");

    ExprKind k = EXPR_MUL;
    if (op == TOK_STAR) k = EXPR_MUL;
    else if (op == TOK_SLASH) k = EXPR_DIVS;
    else if (op == TOK_PERCENT) k = EXPR_MODS;

    Expr* e = new_expr(k, pos);
    e->v.bin.left = left;
    e->v.bin.right = right;
    e->ty = TY_I32;
    left = e;
  }
  return left;
}

static Expr* parse_add(Parser* p, StrPool* sp, ParseCtx* ctx) {
  Expr* left = parse_mul(p, sp, ctx);
  while (tok_is(p, TOK_PLUS) || tok_is(p, TOK_MINUS)) {
    TokKind op = p->cur.kind;
    size_t pos = p->cur.pos;
    advance(p);
    Expr* right = parse_mul(p, sp, ctx);

    if (op == TOK_PLUS && (left->ty == TY_STRING || right->ty == TY_STRING)) {
      require_expr_type(ctx, left, TY_STRING, "string concat left");
      require_expr_type(ctx, right, TY_STRING, "string concat right");
      Expr* e = new_expr(EXPR_ADD, pos);
      e->v.bin.left = left;
      e->v.bin.right = right;
      e->ty = TY_STRING;
      left = e;
      continue;
    }

    // numeric + / -
    require_expr_type(ctx, left, TY_I32, "add/sub left");
    require_expr_type(ctx, right, TY_I32, "add/sub right");

    ExprKind k = (op == TOK_PLUS) ? EXPR_ADD : EXPR_SUB;
    Expr* e = new_expr(k, pos);
    e->v.bin.left = left;
    e->v.bin.right = right;
    e->ty = TY_I32;
    left = e;
  }
  return left;
}

static Expr* parse_cmp(Parser* p, StrPool* sp, ParseCtx* ctx) {
  // comparisons have lower precedence than +-
  Expr* left = parse_add(p, sp, ctx);
  while (tok_is(p, TOK_LT) || tok_is(p, TOK_GT) || tok_is(p, TOK_LE) || tok_is(p, TOK_GE) || tok_is(p, TOK_EQ) || tok_is(p, TOK_NE)) {
    TokKind op = p->cur.kind;
    size_t pos = p->cur.pos;
    advance(p);
    Expr* right = parse_add(p, sp, ctx);

    ExprKind k = EXPR_EQ;
    if (op == TOK_LT) k = EXPR_LT;
    else if (op == TOK_GT) k = EXPR_GT;
    else if (op == TOK_LE) k = EXPR_LE;
    else if (op == TOK_GE) k = EXPR_GE;
    else if (op == TOK_EQ) k = EXPR_EQ;
    else if (op == TOK_NE) k = EXPR_NE;

    // Type rules:
    // - < > <= >= require i32
    // - = != require identical operand types (i32, bool, string)
    if (k == EXPR_LT || k == EXPR_GT || k == EXPR_LE || k == EXPR_GE) {
      require_expr_type(ctx, left, TY_I32, "comparison left");
      require_expr_type(ctx, right, TY_I32, "comparison right");
    } else {
      if (left->ty == 0 && right->ty != 0) set_expr_type(ctx, left, right->ty);
      else if (right->ty == 0 && left->ty != 0) set_expr_type(ctx, right, left->ty);
      else if (left->ty == 0 && right->ty == 0) {
        set_expr_type(ctx, left, TY_I32);
        set_expr_type(ctx, right, TY_I32);
      }
      if (left->ty != right->ty) {
        fprintf(stderr, "zmc: type error (equality): mismatched types %s and %s\n", type_name(left->ty), type_name(right->ty));
        exit(2);
      }
      if (!(left->ty == TY_I32 || left->ty == TY_BOOL || left->ty == TY_STRING)) {
        fprintf(stderr, "zmc: type error (equality): unsupported type %s\n", type_name(left->ty));
        exit(2);
      }
    }

    Expr* e = new_expr(k, pos);
    e->v.bin.left = left;
    e->v.bin.right = right;
    e->ty = TY_BOOL;
    left = e;
  }
  return left;
}

static Expr* parse_and(Parser* p, StrPool* sp, ParseCtx* ctx) {
  Expr* left = parse_cmp(p, sp, ctx);
  while (tok_is(p, TOK_AND)) {
    size_t pos = p->cur.pos;
    advance(p);
    Expr* right = parse_cmp(p, sp, ctx);
    if (left->ty == 0) set_expr_type(ctx, left, TY_I32);
    if (right->ty == 0) set_expr_type(ctx, right, TY_I32);
    if (!(left->ty == TY_BOOL || left->ty == TY_I32)) {
      fprintf(stderr, "zmc: type error (and): left must be bool or i32, got %s\n", type_name(left->ty));
      exit(2);
    }
    if (!(right->ty == TY_BOOL || right->ty == TY_I32)) {
      fprintf(stderr, "zmc: type error (and): right must be bool or i32, got %s\n", type_name(right->ty));
      exit(2);
    }
    Expr* e = new_expr(EXPR_AND, pos);
    e->v.bin.left = left;
    e->v.bin.right = right;
    e->ty = TY_BOOL;
    left = e;
  }
  return left;
}

static Expr* parse_or(Parser* p, StrPool* sp, ParseCtx* ctx) {
  Expr* left = parse_and(p, sp, ctx);
  while (tok_is(p, TOK_OR)) {
    size_t pos = p->cur.pos;
    advance(p);
    Expr* right = parse_and(p, sp, ctx);
    if (left->ty == 0) set_expr_type(ctx, left, TY_I32);
    if (right->ty == 0) set_expr_type(ctx, right, TY_I32);
    if (!(left->ty == TY_BOOL || left->ty == TY_I32)) {
      fprintf(stderr, "zmc: type error (or): left must be bool or i32, got %s\n", type_name(left->ty));
      exit(2);
    }
    if (!(right->ty == TY_BOOL || right->ty == TY_I32)) {
      fprintf(stderr, "zmc: type error (or): right must be bool or i32, got %s\n", type_name(right->ty));
      exit(2);
    }
    Expr* e = new_expr(EXPR_OR, pos);
    e->v.bin.left = left;
    e->v.bin.right = right;
    e->ty = TY_BOOL;
    left = e;
  }
  return left;
}

static Expr* parse_expr(Parser* p, StrPool* sp, ParseCtx* ctx) {
  return parse_or(p, sp, ctx);
}

static void parse_stmt(Parser* p, StrPool* sp, ParseCtx* ctx, StmtList* out);

static StmtList* parse_block(Parser* p, StrPool* sp, ParseCtx* ctx) {
  expect(p, TOK_LBRACE, "'{' ");
  advance(p);
  StmtList* body = stmt_list_new();
  while (!tok_is(p, TOK_RBRACE)) {
    if (tok_is(p, TOK_EOF)) {
      fprintf(stderr, "zmc: expected '}' before EOF\n");
      exit(2);
    }
    parse_stmt(p, sp, ctx, body);
  }
  advance(p);
  return body;
}

static void parse_stmt(Parser* p, StrPool* sp, ParseCtx* ctx, StmtList* out) {
  if (tok_is(p, TOK_LBRACE)) {
    size_t pos = p->cur.pos;
    StmtList* body = parse_block(p, sp, ctx);
    Stmt st;
    memset(&st, 0, sizeof(st));
    st.kind = STMT_BLOCK;
    st.pos = pos;
    st.v.block.body = body;
    stmt_list_push(out, st);
    return;
  }

  if (tok_is(p, TOK_RETURN)) {
    if (!ctx->cur_fn) {
      fprintf(stderr, "zmc: return is only valid inside a function\n");
      exit(2);
    }
    size_t pos = p->cur.pos;
    advance(p);
    Expr* value = parse_expr(p, sp, ctx);
    expect(p, TOK_SEMI, "';'");
    advance(p);

    if (ctx->cur_fn->ret_type == 0 && value->ty != 0) ctx->cur_fn->ret_type = value->ty;
    if (ctx->cur_fn->ret_type != 0) require_expr_type(ctx, value, ctx->cur_fn->ret_type, "return");
    if (ctx->cur_fn->ret_type == 0 && value->ty != 0) ctx->cur_fn->ret_type = value->ty;

    Stmt st;
    memset(&st, 0, sizeof(st));
    st.kind = STMT_RETURN;
    st.pos = pos;
    st.v.ret.value = value;
    stmt_list_push(out, st);
    return;
  }

  if (tok_is(p, TOK_IF)) {
    size_t pos = p->cur.pos;
    advance(p);
    expect(p, TOK_LPAREN, "'('");
    advance(p);
    Expr* cond = parse_expr(p, sp, ctx);
    if (!(cond->ty == TY_BOOL || cond->ty == TY_I32 || cond->ty == 0)) {
      fprintf(stderr, "zmc: type error (if condition): expected bool or i32, got %s\n", type_name(cond->ty));
      exit(2);
    }
    if (cond->ty == 0) set_expr_type(ctx, cond, TY_I32);
    expect(p, TOK_RPAREN, "')'");
    advance(p);

    StmtList* then_body = parse_block(p, sp, ctx);
    StmtList* else_body = NULL;
    if (tok_is(p, TOK_ELSE)) {
      advance(p);
      else_body = parse_block(p, sp, ctx);
    }

    Stmt st;
    memset(&st, 0, sizeof(st));
    st.kind = STMT_IF;
    st.pos = pos;
    st.v.if_.cond = cond;
    st.v.if_.then_body = then_body;
    st.v.if_.else_body = else_body;
    stmt_list_push(out, st);
    return;
  }

  if (tok_is(p, TOK_WHILE)) {
    size_t pos = p->cur.pos;
    advance(p);
    expect(p, TOK_LPAREN, "'('");
    advance(p);
    Expr* cond = parse_expr(p, sp, ctx);
    if (!(cond->ty == TY_BOOL || cond->ty == TY_I32 || cond->ty == 0)) {
      fprintf(stderr, "zmc: type error (while condition): expected bool or i32, got %s\n", type_name(cond->ty));
      exit(2);
    }
    if (cond->ty == 0) set_expr_type(ctx, cond, TY_I32);
    expect(p, TOK_RPAREN, "')'");
    advance(p);
    StmtList* body = parse_block(p, sp, ctx);

    Stmt st;
    memset(&st, 0, sizeof(st));
    st.kind = STMT_WHILE;
    st.pos = pos;
    st.v.while_.cond = cond;
    st.v.while_.body = body;
    stmt_list_push(out, st);
    return;
  }

  if (tok_is(p, TOK_LET) || tok_is(p, TOK_CONST)) {
    bool is_const = tok_is(p, TOK_CONST);
    size_t pos = p->cur.pos;
    advance(p);

    expect(p, TOK_IDENT, "identifier");
    const char* name_ptr = p->src + p->cur.pos;
    size_t name_len = p->cur.len;
    advance(p);

    expect(p, TOK_ASSIGN, "':='");
    advance(p);

    Expr* value = parse_expr(p, sp, ctx);

    expect(p, TOK_SEMI, "';'");
    advance(p);

    RefKind ref = REF_GLOBAL;
    int slot = 0;
    const char* data_label = NULL;
    size_t param_index = 0;

    if (ctx->cur_fn) {
      ref = REF_LOCAL;
      slot = ctx->cur_fn->nlocals;
      Local* l = locals_add(&ctx->cur_fn->locals, name_ptr, name_len, is_const, slot);
      l->ty = value->ty;
      ctx->cur_fn->nlocals++;
    } else {
      Global* g = globals_add(ctx->globals, name_ptr, name_len, is_const);
      globals_set_type(g, value->ty);
      data_label = g->data_label;
    }

    Stmt st;
    memset(&st, 0, sizeof(st));
    st.kind = is_const ? STMT_CONST : STMT_LET;
    st.pos = pos;
    st.v.bind.name = name_ptr;
    st.v.bind.name_len = name_len;
    st.v.bind.value = value;
    st.v.bind.ref = ref;
    st.v.bind.slot = slot;
    st.v.bind.data_label = data_label;
    st.v.bind.param_index = param_index;
    stmt_list_push(out, st);
    return;
  }

  if (tok_is(p, TOK_IDENT) && ident_is(p, "print")) {
    size_t pos = p->cur.pos;
    advance(p);

    expect(p, TOK_LPAREN, "'('");
    advance(p);

    Expr* value = parse_expr(p, sp, ctx);
    if (!(value->ty == TY_STRING || value->ty == TY_I32 || value->ty == 0)) {
      fprintf(stderr, "zmc: type error (print() argument): expected string or i32, got %s\n", type_name(value->ty));
      exit(2);
    }
    if (value->ty == 0) set_expr_type(ctx, value, TY_I32);

    expect(p, TOK_RPAREN, "')'");
    advance(p);

    expect(p, TOK_SEMI, "';'");
    advance(p);

    Stmt st;
    memset(&st, 0, sizeof(st));
    st.kind = STMT_PRINT;
    st.pos = pos;
    st.v.print.value = value;
    stmt_list_push(out, st);
    return;
  }

  // Expression statement or assignment.
  // Parse an expression first; if followed by ':=' treat it as assignment to an identifier.
  Expr* first = parse_expr(p, sp, ctx);
  if (tok_is(p, TOK_ASSIGN)) {
    if (first->kind != EXPR_IDENT) {
      fprintf(stderr, "zmc: left-hand side of ':=' must be an identifier\n");
      exit(2);
    }
    size_t pos = first->pos;
    const char* name_ptr = first->v.ident.name;
    size_t name_len = first->v.ident.name_len;
    free_expr(first);

    advance(p);
    Expr* value = parse_expr(p, sp, ctx);
    expect(p, TOK_SEMI, "';'");
    advance(p);

    RefKind ref = REF_GLOBAL;
    int slot = 0;
    const char* data_label = NULL;
    size_t param_index = 0;
    bool target_is_const = false;
    Type target_type = 0;

    if (ctx->cur_fn) {
      Local* l = locals_find(&ctx->cur_fn->locals, name_ptr, name_len);
      if (l) {
        ref = REF_LOCAL;
        slot = l->slot;
        target_is_const = l->is_const;
        target_type = l->ty;
      } else {
        int pi = fn_param_index(ctx->cur_fn, name_ptr, name_len);
        if (pi >= 0) {
          ref = REF_PARAM;
          param_index = (size_t)pi;
          slot = -(int)(2 + ctx->cur_fn->argc - (size_t)pi);
          target_is_const = false;
          target_type = ctx->cur_fn->param_types[pi];
        }
      }
    }
    if (ref == REF_GLOBAL) {
      const Global* g = globals_find(ctx->globals, name_ptr, name_len);
      if (!g) {
        fprintf(stderr, "zmc: assignment to undefined identifier '%.*s'\n", (int)name_len, name_ptr);
        exit(2);
      }
      target_is_const = g->is_const;
      target_type = (Type)g->ty;
      data_label = g->data_label;
    }

    if (target_is_const) {
      fprintf(stderr, "zmc: cannot assign to const '%.*s'\n", (int)name_len, name_ptr);
      exit(2);
    }
    if (target_type != 0) {
      require_expr_type(ctx, value, target_type, "assignment");
    } else if (value->ty != 0) {
      target_type = value->ty;
      if (ref == REF_LOCAL) {
        Local* l = locals_find(&ctx->cur_fn->locals, name_ptr, name_len);
        if (l && l->ty == 0) l->ty = target_type;
      } else if (ref == REF_PARAM) {
        ctx->cur_fn->param_types[param_index] = target_type;
      }
    }

    Stmt st;
    memset(&st, 0, sizeof(st));
    st.kind = STMT_ASSIGN;
    st.pos = pos;
    st.v.assign.name = name_ptr;
    st.v.assign.name_len = name_len;
    st.v.assign.value = value;
    st.v.assign.ref = ref;
    st.v.assign.slot = slot;
    st.v.assign.data_label = data_label;
    st.v.assign.param_index = param_index;
    st.v.assign.target_is_const = target_is_const;
    st.v.assign.target_type = target_type;
    stmt_list_push(out, st);
    return;
  }

  expect(p, TOK_SEMI, "';'");
  advance(p);
  Stmt st;
  memset(&st, 0, sizeof(st));
  st.kind = STMT_EXPR;
  st.pos = first->pos;
  st.v.expr.value = first;
  stmt_list_push(out, st);
}

static bool stmt_list_has_return(const StmtList* s) {
  for (size_t i = 0; i < s->len; i++) {
    const Stmt* st = &s->items[i];
    if (st->kind == STMT_RETURN) return true;
    if (st->kind == STMT_BLOCK && stmt_list_has_return(st->v.block.body)) return true;
    if (st->kind == STMT_IF) {
      if (stmt_list_has_return(st->v.if_.then_body)) return true;
      if (st->v.if_.else_body && stmt_list_has_return(st->v.if_.else_body)) return true;
    }
    if (st->kind == STMT_WHILE && stmt_list_has_return(st->v.while_.body)) return true;
  }
  return false;
}

static void parse_func_def(Parser* p, StrPool* sp, ParseCtx* ctx) {
  expect(p, TOK_FUNC, "func");
  advance(p);
  expect(p, TOK_IDENT, "function name");
  const char* name_ptr = p->src + p->cur.pos;
  size_t name_len = p->cur.len;
  advance(p);

  expect(p, TOK_LPAREN, "'('");
  advance(p);

  const char** param_ptrs = NULL;
  size_t* param_lens = NULL;
  size_t argc = 0;
  size_t cap = 0;
  if (!tok_is(p, TOK_RPAREN)) {
    for (;;) {
      expect(p, TOK_IDENT, "parameter name");
      if (argc == cap) {
        size_t new_cap = cap ? (cap * 2) : 4;
        param_ptrs = (const char**)xrealloc((void*)param_ptrs, new_cap * sizeof(const char*));
        param_lens = (size_t*)xrealloc(param_lens, new_cap * sizeof(size_t));
        cap = new_cap;
      }
      param_ptrs[argc] = p->src + p->cur.pos;
      param_lens[argc] = p->cur.len;
      argc++;
      advance(p);
      if (tok_is(p, TOK_COMMA)) {
        advance(p);
        continue;
      }
      break;
    }
  }
  expect(p, TOK_RPAREN, "')'");
  advance(p);

  Function* fn = ft_add(ctx->funcs, name_ptr, name_len, argc);
  for (size_t i = 0; i < argc; i++) {
    fn->param_names[i] = (char*)xmalloc(param_lens[i] + 1);
    memcpy(fn->param_names[i], param_ptrs[i], param_lens[i]);
    fn->param_names[i][param_lens[i]] = 0;
  }
  free(param_ptrs);
  free(param_lens);

  Function* prev = ctx->cur_fn;
  ctx->cur_fn = fn;
  fn->body = parse_block(p, sp, ctx);
  ctx->cur_fn = prev;

  if (!stmt_list_has_return(fn->body)) {
    fprintf(stderr, "zmc: function '%s' must contain a return statement (v0)\n", fn->name);
    exit(2);
  }
  if (fn->ret_type == 0) {
    fprintf(stderr, "zmc: could not infer return type for function '%s'\n", fn->name);
    exit(2);
  }
}

static void parse_program(Parser* p, StrPool* sp, ParseCtx* ctx, StmtList* out_main) {
  while (!tok_is(p, TOK_EOF)) {
    if (tok_is(p, TOK_FUNC)) {
      parse_func_def(p, sp, ctx);
      continue;
    }
    parse_stmt(p, sp, ctx, out_main);
  }
}

// ------------------------------ Assembly emission ------------------------------

typedef struct {
  uint32_t next_label_id;
} CodeGen;

static void emit_to_bool(FILE* out, Type ty) {
  if (ty == TY_BOOL) return;
  if (ty == TY_I32) {
    // value != 0 -> 1 else 0
    fprintf(out, "  PUSHI 0\n");
    fprintf(out, "  EQ\n");
    fprintf(out, "  PUSHI 0\n");
    fprintf(out, "  EQ\n");
    return;
  }
  die("internal: emit_to_bool on non-bool/non-i32");
}

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

static void emit_expr_asm(FILE* out, const Expr* e, CodeGen* cg) {
  switch (e->kind) {
    case EXPR_STR_LIT:
      fprintf(out, "  PUSHI %s\n", e->v.str_label);
      return;
    case EXPR_INT_LIT:
      fprintf(out, "  PUSHI %u\n", (unsigned)e->v.int_u32);
      return;
    case EXPR_BOOL_LIT:
      fprintf(out, "  PUSHI %u\n", (unsigned)(e->v.int_u32 ? 1u : 0u));
      return;
    case EXPR_IDENT:
      if (e->v.ident.ref == REF_GLOBAL) {
        fprintf(out, "  PUSHI %s\n", e->v.ident.data_label);
        fprintf(out, "  LOAD32\n");
      } else {
        fprintf(out, "  LDFP %d\n", e->v.ident.slot);
      }
      return;
    case EXPR_CALL:
      if (!e->v.call.fn) die("internal: call expr missing resolved function");
      for (size_t i = 0; i < e->v.call.argc; i++) {
        emit_expr_asm(out, e->v.call.args[i], cg);
      }
      fprintf(out, "  CALL %s\n", e->v.call.fn->label);
      return;
    case EXPR_NEG:
      emit_expr_asm(out, e->v.unary.inner, cg);
      fprintf(out, "  NEG\n");
      return;
    case EXPR_LNOT:
      emit_expr_asm(out, e->v.unary.inner, cg);
      emit_to_bool(out, e->v.unary.inner->ty);
      fprintf(out, "  PUSHI 0\n");
      fprintf(out, "  EQ\n");
      return;
    case EXPR_TEXT:
      emit_expr_asm(out, e->v.unary.inner, cg);
      fprintf(out, "  SYSCALL 8\n");
      return;
    case EXPR_NUMBER:
      emit_expr_asm(out, e->v.unary.inner, cg);
      fprintf(out, "  SYSCALL 9\n");
      return;
    case EXPR_ADD:
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_expr_asm(out, e->v.bin.right, cg);
      if (e->ty == TY_STRING) fprintf(out, "  CALL __zman_strcat\n");
      else fprintf(out, "  ADD\n");
      return;
    case EXPR_SUB:
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_expr_asm(out, e->v.bin.right, cg);
      fprintf(out, "  SUB\n");
      return;
    case EXPR_MUL:
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_expr_asm(out, e->v.bin.right, cg);
      fprintf(out, "  MUL\n");
      return;
    case EXPR_DIVS:
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_expr_asm(out, e->v.bin.right, cg);
      fprintf(out, "  DIVS\n");
      return;
    case EXPR_MODS:
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_expr_asm(out, e->v.bin.right, cg);
      fprintf(out, "  MODS\n");
      return;

    case EXPR_LT:
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_expr_asm(out, e->v.bin.right, cg);
      fprintf(out, "  LT\n");
      return;
    case EXPR_GT:
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_expr_asm(out, e->v.bin.right, cg);
      fprintf(out, "  GT\n");
      return;
    case EXPR_LE:
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_expr_asm(out, e->v.bin.right, cg);
      fprintf(out, "  LE\n");
      return;
    case EXPR_GE:
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_expr_asm(out, e->v.bin.right, cg);
      fprintf(out, "  GE\n");
      return;
    case EXPR_EQ:
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_expr_asm(out, e->v.bin.right, cg);
      fprintf(out, "  EQ\n");
      return;
    case EXPR_NE:
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_expr_asm(out, e->v.bin.right, cg);
      // logical not of EQ result: (eq == 0)
      fprintf(out, "  EQ\n");
      fprintf(out, "  PUSHI 0\n");
      fprintf(out, "  EQ\n");
      return;
    case EXPR_AND: {
      uint32_t id = cg->next_label_id++;
      char end_lbl[64];
      snprintf(end_lbl, sizeof(end_lbl), "L_and_%u_end", (unsigned)id);
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_to_bool(out, e->v.bin.left->ty);
      fprintf(out, "  DUP\n");
      fprintf(out, "  JZ %s\n", end_lbl);
      fprintf(out, "  POP\n");
      emit_expr_asm(out, e->v.bin.right, cg);
      emit_to_bool(out, e->v.bin.right->ty);
      fprintf(out, "%s:\n", end_lbl);
      return;
    }
    case EXPR_OR: {
      uint32_t id = cg->next_label_id++;
      char end_lbl[64];
      snprintf(end_lbl, sizeof(end_lbl), "L_or_%u_end", (unsigned)id);
      emit_expr_asm(out, e->v.bin.left, cg);
      emit_to_bool(out, e->v.bin.left->ty);
      fprintf(out, "  DUP\n");
      fprintf(out, "  JNZ %s\n", end_lbl);
      fprintf(out, "  POP\n");
      emit_expr_asm(out, e->v.bin.right, cg);
      emit_to_bool(out, e->v.bin.right->ty);
      fprintf(out, "%s:\n", end_lbl);
      return;
    }
  }
}

static void emit_stmt_list_asm(FILE* out, const StmtList* list, const Function* cur_fn, CodeGen* cg);

static void emit_stmt_asm(FILE* out, const Stmt* st, const Function* cur_fn, CodeGen* cg) {
  switch (st->kind) {
    case STMT_LET:
    case STMT_CONST: {
      if (st->v.bind.ref == REF_GLOBAL) {
        fprintf(out, "  PUSHI %s\n", st->v.bind.data_label);
        emit_expr_asm(out, st->v.bind.value, cg);
        fprintf(out, "  STORE32\n");
      } else {
        emit_expr_asm(out, st->v.bind.value, cg);
        fprintf(out, "  STFP %d\n", st->v.bind.slot);
      }
      return;
    }
    case STMT_ASSIGN: {
      if (st->v.assign.ref == REF_GLOBAL) {
        fprintf(out, "  PUSHI %s\n", st->v.assign.data_label);
        emit_expr_asm(out, st->v.assign.value, cg);
        fprintf(out, "  STORE32\n");
      } else {
        emit_expr_asm(out, st->v.assign.value, cg);
        fprintf(out, "  STFP %d\n", st->v.assign.slot);
      }
      return;
    }
    case STMT_PRINT:
      emit_expr_asm(out, st->v.print.value, cg);
      if (st->v.print.value->ty == TY_I32) {
        fprintf(out, "  SYSCALL 8\n");
      }
      fprintf(out, "  CALL __zman_print\n");
      fprintf(out, "  POP\n");
      return;
    case STMT_RETURN:
      if (!cur_fn) die("internal: return outside function");
      emit_expr_asm(out, st->v.ret.value, cg);
      fprintf(out, "  RET %zu\n", cur_fn->argc);
      return;
    case STMT_EXPR:
      emit_expr_asm(out, st->v.expr.value, cg);
      fprintf(out, "  POP\n");
      return;
    case STMT_BLOCK:
      emit_stmt_list_asm(out, st->v.block.body, cur_fn, cg);
      return;
    case STMT_IF: {
      uint32_t id = cg->next_label_id++;
      char else_lbl[64];
      char end_lbl[64];
      snprintf(else_lbl, sizeof(else_lbl), "L_if_%u_else", (unsigned)id);
      snprintf(end_lbl, sizeof(end_lbl), "L_if_%u_end", (unsigned)id);

      emit_expr_asm(out, st->v.if_.cond, cg);
      emit_to_bool(out, st->v.if_.cond->ty);
      fprintf(out, "  JZ %s\n", st->v.if_.else_body ? else_lbl : end_lbl);
      emit_stmt_list_asm(out, st->v.if_.then_body, cur_fn, cg);
      if (st->v.if_.else_body) {
        fprintf(out, "  JMP %s\n", end_lbl);
        fprintf(out, "%s:\n", else_lbl);
        emit_stmt_list_asm(out, st->v.if_.else_body, cur_fn, cg);
      }
      fprintf(out, "%s:\n", end_lbl);
      return;
    }
    case STMT_WHILE: {
      uint32_t id = cg->next_label_id++;
      char head_lbl[64];
      char end_lbl[64];
      snprintf(head_lbl, sizeof(head_lbl), "L_while_%u_head", (unsigned)id);
      snprintf(end_lbl, sizeof(end_lbl), "L_while_%u_end", (unsigned)id);
      fprintf(out, "%s:\n", head_lbl);
      emit_expr_asm(out, st->v.while_.cond, cg);
      emit_to_bool(out, st->v.while_.cond->ty);
      fprintf(out, "  JZ %s\n", end_lbl);
      emit_stmt_list_asm(out, st->v.while_.body, cur_fn, cg);
      fprintf(out, "  JMP %s\n", head_lbl);
      fprintf(out, "%s:\n", end_lbl);
      return;
    }
  }
}

static void emit_stmt_list_asm(FILE* out, const StmtList* list, const Function* cur_fn, CodeGen* cg) {
  for (size_t i = 0; i < list->len; i++) {
    emit_stmt_asm(out, &list->items[i], cur_fn, cg);
  }
}

static void emit_function_asm(FILE* out, const Function* fn, CodeGen* cg) {
  fprintf(out, "%s:\n", fn->label);
  fprintf(out, "  ENTER %d\n", fn->nlocals);
  emit_stmt_list_asm(out, fn->body, fn, cg);
  // Should be unreachable (parser enforces at least one return), but keep a
  // hard runtime failure in case control falls off the end.
  fprintf(out, "  TRAP 1\n");
}

static void emit_v0_asm(FILE* out, const StmtList* stmts, const StrPool* sp, const Globals* globals, const FuncTable* funcs) {
  fprintf(out, ".module \"zman_program\"\n\n");
  fprintf(out, ".code\n");
  fprintf(out, ".entry main\n\n");

  // __zman_print(p) -> 0
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

  // __zman_strcat(a,b) -> p
  // Args: a at [fp-4], b at [fp-3]
  fprintf(out, "__zman_strcat:\n");
  fprintf(out, "  ENTER 5\n");
  // len_a
  fprintf(out, "  LDFP -4\n");
  fprintf(out, "  DUP\n");
  fprintf(out, "  LOAD32\n");
  fprintf(out, "  STFP 0\n");
  fprintf(out, "  POP\n");
  // len_b
  fprintf(out, "  LDFP -3\n");
  fprintf(out, "  DUP\n");
  fprintf(out, "  LOAD32\n");
  fprintf(out, "  STFP 1\n");
  fprintf(out, "  POP\n");
  // total
  fprintf(out, "  LDFP 0\n");
  fprintf(out, "  LDFP 1\n");
  fprintf(out, "  ADD\n");
  fprintf(out, "  STFP 2\n");
  // alloc ptr
  fprintf(out, "  LDFP 2\n");
  fprintf(out, "  ADDI 4\n");
  fprintf(out, "  SYSCALL 6\n");
  fprintf(out, "  DUP\n");
  fprintf(out, "  STFP 3\n");
  // store total at ptr
  fprintf(out, "  DUP\n");
  fprintf(out, "  LDFP 2\n");
  fprintf(out, "  STORE32\n");
  // dest_data = ptr + 4
  fprintf(out, "  DUP\n");
  fprintf(out, "  ADDI 4\n");
  fprintf(out, "  STFP 4\n");
  fprintf(out, "  POP\n");
  // memcpy(dest_data, a+4, len_a)
  fprintf(out, "  LDFP 4\n");
  fprintf(out, "  LDFP -4\n");
  fprintf(out, "  ADDI 4\n");
  fprintf(out, "  LDFP 0\n");
  fprintf(out, "  MEMCPY\n");
  // memcpy(dest_data+len_a, b+4, len_b)
  fprintf(out, "  LDFP 4\n");
  fprintf(out, "  LDFP 0\n");
  fprintf(out, "  ADD\n");
  fprintf(out, "  LDFP -3\n");
  fprintf(out, "  ADDI 4\n");
  fprintf(out, "  LDFP 1\n");
  fprintf(out, "  MEMCPY\n");
  // return ptr
  fprintf(out, "  LDFP 3\n");
  fprintf(out, "  RET 2\n\n");

  CodeGen cg;
  cg.next_label_id = 0;

  // user functions
  for (size_t i = 0; i < funcs->len; i++) {
    emit_function_asm(out, &funcs->items[i], &cg);
    fprintf(out, "\n");
  }

  fprintf(out, "main:\n");
  emit_stmt_list_asm(out, stmts, NULL, &cg);
  fprintf(out, "  HALT\n\n");

  fprintf(out, ".data\n");
  fprintf(out, "  .align 4\n\n");

  // globals
  for (size_t i = 0; i < globals->len; i++) {
    fprintf(out, "%s:\n", globals->items[i].data_label);
    fprintf(out, "  .word 0\n");
  }
  if (globals->len > 0) fprintf(out, "\n");

  // string literals
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
          "  - let/const <name> := <expr>;\n"
          "  - <name> := <expr>;\n"
          "  - print(<string-expr>);\n"
          "  - if (<expr>) { ... } else { ... }\n"
          "  - while (<expr>) { ... }\n"
          "  - func <name>(<params...>) { ... }\n"
          "  - return <expr>;\n"
          "  - expressions: string literals, integer literals, identifiers, parentheses\n"
          "    - string: + (concatenation), text(<int-expr>)\n"
          "    - int: + - * / %% and unary -, number(<string-expr>)\n"
          "    - comparisons: < > <= >= = != (produce bool)\n"
          "    - booleans: true, false, !x / not x, x and y, x or y\n");
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

  Globals globals;
  globals_init(&globals);

  FuncTable funcs;
  ft_init(&funcs);

  StmtList* stmts = stmt_list_new();

  ParseCtx ctx;
  ctx.globals = &globals;
  ctx.funcs = &funcs;
  ctx.cur_fn = NULL;
  parse_program(&p, &sp, &ctx, stmts);
  token_free(&p.cur);

  FILE* out = fopen(out_path, "wb");
  if (!out) {
    fprintf(stderr, "zmc: failed to open '%s' for writing: %s\n", out_path, strerror(errno));
    return 2;
  }

  emit_v0_asm(out, stmts, &sp, &globals, &funcs);
  fclose(out);

  stmt_list_free(stmts);
  ft_free(&funcs);
  globals_free(&globals);
  sp_free(&sp);
  free(src);

  return 0;
}
