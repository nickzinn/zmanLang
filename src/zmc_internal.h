#ifndef ZMC_INTERNAL_H
#define ZMC_INTERNAL_H

// Purpose: Shared internal declarations for the multi-file zmc compiler.

#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
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

void die(const char* msg);
void* xmalloc(size_t n);
void* xrealloc(void* p, size_t n);

void bb_init(ByteBuf* b);
void bb_free(ByteBuf* b);
void bb_reserve(ByteBuf* b, size_t need);
void bb_push(ByteBuf* b, uint8_t v);

char* read_entire_file(const char* path, size_t* out_len);

// ------------------------------ Lexer ------------------------------

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

void lex_init(Lexer* lx, const char* src, size_t len);
Token next_token(Lexer* lx);
void token_free(Token* t);

// ------------------------------ IR / AST ------------------------------

typedef struct {
  char* label;
  ByteBuf bytes; // string payload bytes
} StrLit;

typedef struct {
  StrLit* items;
  size_t len;
  size_t cap;
} StrPool;

void sp_init(StrPool* sp);
void sp_free(StrPool* sp);
const char* sp_add(StrPool* sp, const ByteBuf* bytes);

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

void globals_init(Globals* g);
void globals_free(Globals* g);
const Global* globals_find(const Globals* g, const char* name, size_t name_len);
Global* globals_add(Globals* g, const char* name, size_t name_len, bool is_const);

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

const char* type_name(Type t);
void globals_set_type(Global* g, Type ty);

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
    uint32_t int_u32;      // EXPR_INT_LIT / EXPR_BOOL_LIT
    struct {
      const char* name;
      size_t name_len;
      RefKind ref;
      int slot;              // for locals/params: fp+slot (slot may be negative for params)
      const char* data_label; // for globals
      size_t param_index;    // for params
    } ident; // EXPR_IDENT
    struct {
      const char* name;
      size_t name_len;
      Expr** args;
      size_t argc;
      Function* fn; // resolved callee
    } call; // EXPR_CALL
    struct { Expr* left; Expr* right; } bin;   // binary
    struct { Expr* inner; } unary;             // unary
  } v;
};

Expr* new_expr(ExprKind k, size_t pos);
void free_expr(Expr* e);

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
    } bind; // let/const
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
    struct { Expr* cond; StmtList* then_body; StmtList* else_body; } if_;
    struct { Expr* cond; StmtList* body; } while_;
    struct { StmtList* body; } block;
  } v;
} Stmt;

struct StmtList {
  Stmt* items;
  size_t len;
  size_t cap;
};

StmtList* stmt_list_new(void);
void stmt_list_free(StmtList* s);
void stmt_list_push(StmtList* s, Stmt st);

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

void locals_init(Locals* l);
void locals_free(Locals* l);
Local* locals_find(Locals* l, const char* name, size_t name_len);
Local* locals_add(Locals* l, const char* name, size_t name_len, bool is_const, int slot);

struct Function {
  char* name;
  char* label;
  size_t argc;
  Type* param_types;  // length argc
  char** param_names; // length argc
  Type ret_type;      // 0 until inferred
  Locals locals;
  int nlocals;
  StmtList* body;
};

typedef struct {
  Function* items;
  size_t len;
  size_t cap;
} FuncTable;

void ft_init(FuncTable* ft);
void ft_free(FuncTable* ft);
Function* ft_find(FuncTable* ft, const char* name, size_t name_len);
Function* ft_add(FuncTable* ft, const char* name, size_t name_len, size_t argc);

// ------------------------------ Parser ------------------------------

typedef struct {
  Lexer lx;
  Token cur;
  const char* src;
} Parser;

typedef struct {
  Globals* globals;
  FuncTable* funcs;
  Function* cur_fn; // NULL at top-level
} ParseCtx;

void parse_init(Parser* p, const char* src, size_t len);
void advance(Parser* p);
bool tok_is(Parser* p, TokKind k);
void expect(Parser* p, TokKind k, const char* what);
bool ident_is(Parser* p, const char* s);

void parse_program(Parser* p, StrPool* sp, ParseCtx* ctx, StmtList* out_main);

// ------------------------------ Codegen ------------------------------

void emit_v0_asm(FILE* out, const StmtList* stmts, const StrPool* sp, const Globals* globals, const FuncTable* funcs);

#endif
