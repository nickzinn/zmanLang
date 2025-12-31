#include "zmc_internal.h"

// Purpose: Recursive-descent parser for ZManLang (builds AST, performs basic type inference, resolves names).

// ------------------------------ Parser helpers ------------------------------

void parse_init(Parser* p, const char* src, size_t len) {
  p->src = src;
  lex_init(&p->lx, src, len);
  p->cur = next_token(&p->lx);
}

void advance(Parser* p) {
  token_free(&p->cur);
  p->cur = next_token(&p->lx);
}

bool tok_is(Parser* p, TokKind k) { return p->cur.kind == k; }

void expect(Parser* p, TokKind k, const char* what) {
  if (!tok_is(p, k)) {
    zmc_failf("zmc: expected %s", what);
  }
}

bool ident_is(Parser* p, const char* s) {
  size_t n = strlen(s);
  if (!tok_is(p, TOK_IDENT) || p->cur.len != n) return false;
  return memcmp(p->src + p->cur.pos, s, n) == 0;
}

// ------------------------------ Type inference / name resolution ------------------------------

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
    zmc_failf("zmc: type error (%s): expected %s, got %s", where, type_name(want), type_name(e->ty));
  }
}

static void resolve_ident_expr(ParseCtx* ctx, Expr* e) {
  if (!ctx->cur_fn) {
    const Global* g = globals_find(ctx->globals, e->v.ident.name, e->v.ident.name_len);
    if (!g || g->ty == 0) {
      zmc_failf("zmc: undefined identifier '%.*s'", (int)e->v.ident.name_len, e->v.ident.name);
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
    zmc_failf("zmc: undefined identifier '%.*s'", (int)e->v.ident.name_len, e->v.ident.name);
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
    zmc_failf("zmc: type error: expected %s, got %s", type_name(ty), type_name(e->ty));
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

// ------------------------------ Expression parsing ------------------------------

static Expr* parse_expr(Parser* p, StrPool* sp, ParseCtx* ctx);
static StmtList* parse_block(Parser* p, StrPool* sp, ParseCtx* ctx);

static bool parse_binding_suffix_array(Parser* p) {
  if (!tok_is(p, TOK_LBRACK)) return false;
  advance(p);
  expect(p, TOK_RBRACK, "']'");
  advance(p);
  return true;
}

static Expr* parse_primary(Parser* p, StrPool* sp, ParseCtx* ctx) {
  // Array allocation: [n]
  if (tok_is(p, TOK_LBRACK)) {
    size_t pos = p->cur.pos;
    advance(p);
    Expr* n = parse_expr(p, sp, ctx);
    require_expr_type(ctx, n, TY_I32, "array length");
    expect(p, TOK_RBRACK, "']'");
    advance(p);
    Expr* e = new_expr(EXPR_ARRAY_ALLOC, pos);
    e->v.unary.inner = n;
    e->ty = TY_ARRAY_I32;
    return e;
  }

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

  if (ident_is(p, "length")) {
    size_t pos = p->cur.pos;
    advance(p);
    expect(p, TOK_LPAREN, "'('");
    advance(p);
    Expr* inner = parse_expr(p, sp, ctx);
    if (inner->ty == 0) {
      zmc_failf("zmc: could not infer type for length() argument");
    }
    if (!(inner->ty == TY_STRING || inner->ty == TY_ARRAY_I32)) {
      zmc_failf("zmc: type error (length() argument): expected string or array, got %s", type_name(inner->ty));
    }
    expect(p, TOK_RPAREN, "')'");
    advance(p);
    Expr* e = new_expr(EXPR_LENGTH, pos);
    e->v.unary.inner = inner;
    e->ty = TY_I32;
    return e;
  }

  if (tok_is(p, TOK_IDENT)) {
    size_t pos = p->cur.pos;
    const char* name_ptr = p->src + p->cur.pos;
    size_t name_len = p->cur.len;
    advance(p);

    // Call
    if (tok_is(p, TOK_LPAREN)) {
      Expr* call = new_expr(EXPR_CALL, pos);
      call->v.call.name = name_ptr;
      call->v.call.name_len = name_len;
      call->v.call.args = NULL;
      call->v.call.argc = 0;
      call->v.call.fn = NULL;

      advance(p);
      if (!tok_is(p, TOK_RPAREN)) {
        for (;;) {
          Expr* arg = parse_expr(p, sp, ctx);
          size_t idx = call->v.call.argc++;
          call->v.call.args = (Expr**)xrealloc(call->v.call.args, call->v.call.argc * sizeof(Expr*));
          call->v.call.args[idx] = arg;
          if (tok_is(p, TOK_COMMA)) {
            advance(p);
            continue;
          }
          break;
        }
      }
      expect(p, TOK_RPAREN, "')'");
      advance(p);

      Function* fn = ft_find(ctx->funcs, name_ptr, name_len);
      if (!fn) {
        zmc_failf("zmc: call to undefined function '%.*s'", (int)name_len, name_ptr);
      }
      if (fn->argc != call->v.call.argc) {
        zmc_failf("zmc: function '%s' expects %zu args but got %zu", fn->name, fn->argc, call->v.call.argc);
      }
      call->v.call.fn = fn;

      // unify/infer arg types
      for (size_t i = 0; i < fn->argc; i++) {
        Expr* a = call->v.call.args[i];
        if (fn->param_types[i] != 0) {
          require_expr_type(ctx, a, fn->param_types[i], "call argument");
        } else if (a->ty != 0) {
          fn->param_types[i] = a->ty;
        }
      }

      call->ty = fn->ret_type;
      return call;
    }

    // Identifier
    Expr* e = new_expr(EXPR_IDENT, pos);
    e->v.ident.name = name_ptr;
    e->v.ident.name_len = name_len;
    e->v.ident.ref = REF_GLOBAL;
    e->v.ident.slot = 0;
    e->v.ident.data_label = NULL;
    e->v.ident.param_index = 0;
    resolve_ident_expr(ctx, e);
    return e;
  }

  if (tok_is(p, TOK_STRING)) {
    size_t pos = p->cur.pos;
    const char* label = sp_add(sp, &p->cur.str_bytes);
    Expr* e = new_expr(EXPR_STR_LIT, pos);
    e->v.str_label = label;
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

  if (tok_is(p, TOK_LPAREN)) {
    advance(p);
    Expr* e = parse_expr(p, sp, ctx);
    expect(p, TOK_RPAREN, "')'");
    advance(p);
    return e;
  }

  zmc_failf("zmc: expected expression at byte %zu", p->cur.pos);
}

static Expr* parse_postfix(Parser* p, StrPool* sp, ParseCtx* ctx) {
  Expr* left = parse_primary(p, sp, ctx);
  for (;;) {
    if (tok_is(p, TOK_LBRACK)) {
      size_t pos = p->cur.pos;
      advance(p);
      Expr* idx = parse_expr(p, sp, ctx);
      require_expr_type(ctx, idx, TY_I32, "index");
      expect(p, TOK_RBRACK, "']'");
      advance(p);

      if (left->ty == 0) set_expr_type(ctx, left, TY_ARRAY_I32);
      if (left->ty != TY_ARRAY_I32) {
        zmc_failf("zmc: type error (index): expected array, got %s", type_name(left->ty));
      }

      Expr* e = new_expr(EXPR_INDEX, pos);
      e->v.index.base = left;
      e->v.index.index = idx;
      e->ty = TY_I32;
      left = e;
      continue;
    }
    break;
  }
  return left;
}

static Expr* parse_unary(Parser* p, StrPool* sp, ParseCtx* ctx) {
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
  if (tok_is(p, TOK_BANG) || tok_is(p, TOK_NOT)) {
    size_t pos = p->cur.pos;
    advance(p);
    Expr* inner = parse_unary(p, sp, ctx);
    if (inner->ty == 0) set_expr_type(ctx, inner, TY_I32);
    if (!(inner->ty == TY_BOOL || inner->ty == TY_I32)) {
      zmc_failf("zmc: type error (!/not): expected bool or i32, got %s", type_name(inner->ty));
    }
    Expr* e = new_expr(EXPR_LNOT, pos);
    e->v.unary.inner = inner;
    e->ty = TY_BOOL;
    return e;
  }
  return parse_postfix(p, sp, ctx);
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
        zmc_failf("zmc: type error (equality): mismatched types %s and %s", type_name(left->ty), type_name(right->ty));
      }
      if (!(left->ty == TY_I32 || left->ty == TY_BOOL || left->ty == TY_STRING || left->ty == TY_ARRAY_I32)) {
        zmc_failf("zmc: type error (equality): unsupported type %s", type_name(left->ty));
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
    if (!((left->ty == TY_BOOL || left->ty == TY_I32) && (right->ty == TY_BOOL || right->ty == TY_I32))) {
      zmc_failf("zmc: type error (and): expected bool or i32 operands");
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
    if (!((left->ty == TY_BOOL || left->ty == TY_I32) && (right->ty == TY_BOOL || right->ty == TY_I32))) {
      zmc_failf("zmc: type error (or): expected bool or i32 operands");
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

// ------------------------------ Statement parsing ------------------------------

static StmtList* parse_block(Parser* p, StrPool* sp, ParseCtx* ctx);
static void parse_stmt(Parser* p, StrPool* sp, ParseCtx* ctx, StmtList* out);

static StmtList* parse_block(Parser* p, StrPool* sp, ParseCtx* ctx) {
  expect(p, TOK_LBRACE, "'{' block");
  advance(p);
  StmtList* body = stmt_list_new();
  while (!tok_is(p, TOK_RBRACE)) {
    if (tok_is(p, TOK_EOF)) {
      zmc_failf("zmc: unexpected EOF in block");
    }
    parse_stmt(p, sp, ctx, body);
  }
  expect(p, TOK_RBRACE, "'}'");
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
      zmc_failf("zmc: return is only valid inside a function");
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
      zmc_failf("zmc: type error (if condition): expected bool or i32, got %s", type_name(cond->ty));
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
      zmc_failf("zmc: type error (while condition): expected bool or i32, got %s", type_name(cond->ty));
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

  if (tok_is(p, TOK_FOREACH)) {
    if (!ctx->cur_fn) {
      zmc_failf("zmc: foreach is only valid inside a function");
    }
    size_t pos = p->cur.pos;
    advance(p);

    expect(p, TOK_LPAREN, "'('");
    advance(p);

    expect(p, TOK_IDENT, "loop variable name");
    const char* name_ptr = p->src + p->cur.pos;
    size_t name_len = p->cur.len;
    advance(p);

    expect(p, TOK_COMMA, "','");
    advance(p);

    Expr* array = parse_expr(p, sp, ctx);
    require_expr_type(ctx, array, TY_ARRAY_I32, "foreach array");

    // Bind loop variable after parsing the array expression so it isn't visible there.
    int var_slot = ctx->cur_fn->nlocals;
    Local* l = locals_add(&ctx->cur_fn->locals, name_ptr, name_len, false, var_slot);
    l->ty = TY_I32;
    ctx->cur_fn->nlocals++;

    // Internal locals: evaluated array pointer and current index.
    int arr_slot = ctx->cur_fn->nlocals++;
    int idx_slot = ctx->cur_fn->nlocals++;

    expect(p, TOK_RPAREN, "')'");
    advance(p);

    StmtList* body = parse_block(p, sp, ctx);

    Stmt st;
    memset(&st, 0, sizeof(st));
    st.kind = STMT_FOREACH;
    st.pos = pos;
    st.v.foreach_.array = array;
    st.v.foreach_.var_slot = var_slot;
    st.v.foreach_.arr_slot = arr_slot;
    st.v.foreach_.idx_slot = idx_slot;
    st.v.foreach_.body = body;
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

    bool hint_array = false;
    if (tok_is(p, TOK_LBRACK)) hint_array = parse_binding_suffix_array(p);

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
      if (hint_array) {
        require_expr_type(ctx, value, TY_ARRAY_I32, "array binding initializer");
        l->ty = TY_ARRAY_I32;
      } else {
        l->ty = value->ty;
      }
      ctx->cur_fn->nlocals++;
    } else {
      Global* g = globals_add(ctx->globals, name_ptr, name_len, is_const);
      if (hint_array) {
        require_expr_type(ctx, value, TY_ARRAY_I32, "array binding initializer");
        globals_set_type(g, TY_ARRAY_I32);
      } else {
        globals_set_type(g, value->ty);
      }
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
      zmc_failf("zmc: type error (print() argument): expected string or i32, got %s", type_name(value->ty));
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
  Expr* first = parse_expr(p, sp, ctx);
  if (tok_is(p, TOK_ASSIGN)) {
    size_t pos = first->pos;

    advance(p);
    Expr* value = parse_expr(p, sp, ctx);
    expect(p, TOK_SEMI, "';'");
    advance(p);

    if (first->kind == EXPR_INDEX) {
      // array[index] := value;
      require_expr_type(ctx, first->v.index.base, TY_ARRAY_I32, "array store base");
      require_expr_type(ctx, first->v.index.index, TY_I32, "array store index");
      require_expr_type(ctx, value, TY_I32, "array store value");

      Stmt st;
      memset(&st, 0, sizeof(st));
      st.kind = STMT_ASTORE;
      st.pos = pos;
      st.v.astore.target = first;
      st.v.astore.value = value;
      stmt_list_push(out, st);
      return;
    }

    if (first->kind != EXPR_IDENT) {
      zmc_failf("zmc: left-hand side of ':=' must be an identifier or index expression");
    }

    const char* name_ptr = first->v.ident.name;
    size_t name_len = first->v.ident.name_len;
    free_expr(first);

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
        zmc_failf("zmc: assignment to undefined identifier '%.*s'", (int)name_len, name_ptr);
      }
      target_is_const = g->is_const;
      target_type = (Type)g->ty;
      data_label = g->data_label;
    }

    if (target_is_const) {
      zmc_failf("zmc: cannot assign to const '%.*s'", (int)name_len, name_ptr);
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
    if (st->kind == STMT_FOREACH && stmt_list_has_return(st->v.foreach_.body)) return true;
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
  bool* param_is_array = NULL;
  size_t argc = 0;
  size_t cap = 0;
  if (!tok_is(p, TOK_RPAREN)) {
    for (;;) {
      expect(p, TOK_IDENT, "parameter name");
      if (argc == cap) {
        size_t new_cap = cap ? (cap * 2) : 4;
        param_ptrs = (const char**)xrealloc((void*)param_ptrs, new_cap * sizeof(const char*));
        param_lens = (size_t*)xrealloc(param_lens, new_cap * sizeof(size_t));
        param_is_array = (bool*)xrealloc(param_is_array, new_cap * sizeof(bool));
        cap = new_cap;
      }
      param_ptrs[argc] = p->src + p->cur.pos;
      param_lens[argc] = p->cur.len;
      argc++;
      advance(p);

      bool hint_array = false;
      if (tok_is(p, TOK_LBRACK)) hint_array = parse_binding_suffix_array(p);
      param_is_array[argc - 1] = hint_array;
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
    if (param_is_array && param_is_array[i]) fn->param_types[i] = TY_ARRAY_I32;
  }
  free(param_ptrs);
  free(param_lens);
  free(param_is_array);

  Function* prev = ctx->cur_fn;
  ctx->cur_fn = fn;
  fn->body = parse_block(p, sp, ctx);
  ctx->cur_fn = prev;

  if (!stmt_list_has_return(fn->body)) {
    zmc_failf("zmc: function '%s' must contain a return statement (v0)", fn->name);
  }
  if (fn->ret_type == 0) {
    zmc_failf("zmc: could not infer return type for function '%s'", fn->name);
  }
}

void parse_program(Parser* p, StrPool* sp, ParseCtx* ctx, StmtList* out_main) {
  while (!tok_is(p, TOK_EOF)) {
    if (tok_is(p, TOK_FUNC)) {
      parse_func_def(p, sp, ctx);
      continue;
    }
    parse_stmt(p, sp, ctx, out_main);
  }
}
