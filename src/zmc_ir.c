#include "zmc_internal.h"

// Purpose: IR/AST data structures and helpers for zmc (string pool, globals, locals, function table, AST allocation).

// ------------------------------ String pool ------------------------------

void sp_init(StrPool* sp) {
  sp->items = NULL;
  sp->len = 0;
  sp->cap = 0;
}

void sp_free(StrPool* sp) {
  for (size_t i = 0; i < sp->len; i++) {
    free(sp->items[i].label);
    bb_free(&sp->items[i].bytes);
  }
  free(sp->items);
  sp->items = NULL;
  sp->len = 0;
  sp->cap = 0;
}

const char* sp_add(StrPool* sp, const ByteBuf* bytes) {
  // Intern identical strings: if we already have an entry with the same byte
  // payload, reuse its label to avoid emitting duplicate literals.
  for (size_t i = 0; i < sp->len; i++) {
    const StrLit* it = &sp->items[i];
    if (it->bytes.len != bytes->len) continue;
    if (bytes->len == 0) return it->label;
    if (memcmp(it->bytes.data, bytes->data, bytes->len) == 0) return it->label;
  }

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

// ------------------------------ Globals ------------------------------

void globals_init(Globals* g) {
  g->items = NULL;
  g->len = 0;
  g->cap = 0;
}

void globals_free(Globals* g) {
  for (size_t i = 0; i < g->len; i++) {
    free(g->items[i].name);
    free(g->items[i].data_label);
  }
  free(g->items);
  g->items = NULL;
  g->len = 0;
  g->cap = 0;
}

const Global* globals_find(const Globals* g, const char* name, size_t name_len) {
  for (size_t i = 0; i < g->len; i++) {
    if (strlen(g->items[i].name) == name_len && memcmp(g->items[i].name, name, name_len) == 0) return &g->items[i];
  }
  return NULL;
}

Global* globals_add(Globals* g, const char* name, size_t name_len, bool is_const) {
  if (globals_find(g, name, name_len)) {
    zmc_failf("zmc: duplicate global '%.*s'", (int)name_len, name);
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

const char* type_name(Type t) {
  switch (t) {
    case TY_STRING: return "string";
    case TY_I32: return "i32";
    case TY_BOOL: return "bool";
    case TY_ARRAY_I32: return "array[i32]";
    case TY_ARRAY_STRING: return "array[string]";
    default: return "<unknown>";
  }
}

void globals_set_type(Global* g, Type ty) {
  g->ty = (int)ty;
}

// ------------------------------ AST memory ------------------------------

Expr* new_expr(ExprKind k, size_t pos) {
  Expr* e = (Expr*)xmalloc(sizeof(Expr));
  memset(e, 0, sizeof(*e));
  e->kind = k;
  e->pos = pos;
  e->ty = 0;
  return e;
}

void free_expr(Expr* e) {
  if (!e) return;
  switch (e->kind) {
    case EXPR_CALL:
      for (size_t i = 0; i < e->v.call.argc; i++) free_expr(e->v.call.args[i]);
      free(e->v.call.args);
      break;
    case EXPR_ARRAY_ALLOC:
    case EXPR_LENGTH:
      free_expr(e->v.unary.inner);
      break;
    case EXPR_INDEX:
      free_expr(e->v.index.base);
      free_expr(e->v.index.index);
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
    case EXPR_TEXT:
    case EXPR_NUMBER:
      free_expr(e->v.unary.inner);
      break;
    default:
      break;
  }
  free(e);
}

static void stmt_free(Stmt* st);

StmtList* stmt_list_new(void) {
  StmtList* s = (StmtList*)xmalloc(sizeof(StmtList));
  s->items = NULL;
  s->len = 0;
  s->cap = 0;
  return s;
}

void stmt_list_free(StmtList* s) {
  if (!s) return;
  for (size_t i = 0; i < s->len; i++) stmt_free(&s->items[i]);
  free(s->items);
  free(s);
}

void stmt_list_push(StmtList* s, Stmt st) {
  if (s->len == s->cap) {
    size_t new_cap = s->cap ? (s->cap * 2) : 32;
    s->items = (Stmt*)xrealloc(s->items, new_cap * sizeof(Stmt));
    s->cap = new_cap;
  }
  s->items[s->len++] = st;
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
    case STMT_ASTORE:
      free_expr(st->v.astore.target);
      free_expr(st->v.astore.value);
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
    case STMT_FOREACH:
      free_expr(st->v.foreach_.array);
      stmt_list_free(st->v.foreach_.body);
      return;
    case STMT_BLOCK:
      stmt_list_free(st->v.block.body);
      return;
  }
}

// ------------------------------ Locals ------------------------------

void locals_init(Locals* l) {
  l->items = NULL;
  l->len = 0;
  l->cap = 0;
}

void locals_free(Locals* l) {
  for (size_t i = 0; i < l->len; i++) free(l->items[i].name);
  free(l->items);
  l->items = NULL;
  l->len = 0;
  l->cap = 0;
}

Local* locals_find(Locals* l, const char* name, size_t name_len) {
  for (size_t i = 0; i < l->len; i++) {
    if (strlen(l->items[i].name) == name_len && memcmp(l->items[i].name, name, name_len) == 0) return &l->items[i];
  }
  return NULL;
}

Local* locals_add(Locals* l, const char* name, size_t name_len, bool is_const, int slot) {
  if (locals_find(l, name, name_len)) {
    zmc_failf("zmc: duplicate local '%.*s'", (int)name_len, name);
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

// ------------------------------ Function table ------------------------------

void ft_init(FuncTable* ft) {
  ft->items = NULL;
  ft->len = 0;
  ft->cap = 0;
}

void ft_free(FuncTable* ft) {
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

Function* ft_find(FuncTable* ft, const char* name, size_t name_len) {
  for (size_t i = 0; i < ft->len; i++) {
    if (strlen(ft->items[i].name) == name_len && memcmp(ft->items[i].name, name, name_len) == 0) return &ft->items[i];
  }
  return NULL;
}

Function* ft_add(FuncTable* ft, const char* name, size_t name_len, size_t argc) {
  if (ft_find(ft, name, name_len)) {
    zmc_failf("zmc: duplicate function '%.*s'", (int)name_len, name);
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
