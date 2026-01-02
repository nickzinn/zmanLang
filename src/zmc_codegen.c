#include "zmc_internal.h"

// Purpose: Emit StackVM-32 assembly from the parsed program/AST (including runtime helpers).

// The codegen is written in a FILE*-style, but for WASM we need in-memory output.
// We keep the emitter readable by mapping fprintf/fputs/fputc to ByteBuf helpers.
#undef fprintf
#undef fputs
#undef fputc
#define fprintf(out, ...) bb_printf((out), __VA_ARGS__)
#define fputs(s, out) bb_write_str((out), (s))
#define fputc(c, out) bb_push((out), (uint8_t)(c))

typedef struct {
  uint32_t next_label_id;
  uint32_t used_helpers;
} CodeGen;

typedef enum {
  RT_HELPER_PRINT      = 1u << 0,
  RT_HELPER_STRCAT     = 1u << 1,
  RT_HELPER_ARRAY_ALLOC= 1u << 2,
  RT_HELPER_AGET       = 1u << 3,
  RT_HELPER_ASET       = 1u << 4,
} RuntimeHelper;

static void mark_helpers_expr(const Expr* e, CodeGen* cg);
static void mark_helpers_stmt_list(const StmtList* list, CodeGen* cg);

static void mark_helpers_expr(const Expr* e, CodeGen* cg) {
  if (!e) return;
  switch (e->kind) {
    case EXPR_STR_LIT:
    case EXPR_INT_LIT:
    case EXPR_BOOL_LIT:
    case EXPR_IDENT:
      return;
    case EXPR_CALL:
      for (size_t i = 0; i < e->v.call.argc; i++) {
        mark_helpers_expr(e->v.call.args[i], cg);
      }
      return;
    case EXPR_ARRAY_ALLOC:
      cg->used_helpers |= RT_HELPER_ARRAY_ALLOC;
      mark_helpers_expr(e->v.unary.inner, cg);
      return;
    case EXPR_INDEX:
      cg->used_helpers |= RT_HELPER_AGET;
      mark_helpers_expr(e->v.index.base, cg);
      mark_helpers_expr(e->v.index.index, cg);
      return;
    case EXPR_LENGTH:
    case EXPR_NEG:
    case EXPR_LNOT:
    case EXPR_TEXT:
    case EXPR_NUMBER:
      mark_helpers_expr(e->v.unary.inner, cg);
      return;

    case EXPR_ADD:
      if (e->ty == TY_STRING) cg->used_helpers |= RT_HELPER_STRCAT;
      mark_helpers_expr(e->v.bin.left, cg);
      mark_helpers_expr(e->v.bin.right, cg);
      return;
    case EXPR_SUB:
    case EXPR_MUL:
    case EXPR_DIVS:
    case EXPR_MODS:
    case EXPR_LT:
    case EXPR_GT:
    case EXPR_LE:
    case EXPR_GE:
    case EXPR_EQ:
    case EXPR_NE:
    case EXPR_AND:
    case EXPR_OR:
      mark_helpers_expr(e->v.bin.left, cg);
      mark_helpers_expr(e->v.bin.right, cg);
      return;
  }
}

static void mark_helpers_stmt(const Stmt* st, CodeGen* cg) {
  switch (st->kind) {
    case STMT_LET:
    case STMT_CONST:
      mark_helpers_expr(st->v.bind.value, cg);
      return;
    case STMT_ASSIGN:
      mark_helpers_expr(st->v.assign.value, cg);
      return;
    case STMT_ASTORE:
      cg->used_helpers |= RT_HELPER_ASET;
      if (st->v.astore.target && st->v.astore.target->kind == EXPR_INDEX) {
        mark_helpers_expr(st->v.astore.target->v.index.base, cg);
        mark_helpers_expr(st->v.astore.target->v.index.index, cg);
      } else {
        mark_helpers_expr(st->v.astore.target, cg);
      }
      mark_helpers_expr(st->v.astore.value, cg);
      return;
    case STMT_PRINT:
      cg->used_helpers |= RT_HELPER_PRINT;
      mark_helpers_expr(st->v.print.value, cg);
      return;
    case STMT_RETURN:
      mark_helpers_expr(st->v.ret.value, cg);
      return;
    case STMT_EXPR:
      mark_helpers_expr(st->v.expr.value, cg);
      return;
    case STMT_BLOCK:
      mark_helpers_stmt_list(st->v.block.body, cg);
      return;
    case STMT_IF:
      mark_helpers_expr(st->v.if_.cond, cg);
      mark_helpers_stmt_list(st->v.if_.then_body, cg);
      if (st->v.if_.else_body) mark_helpers_stmt_list(st->v.if_.else_body, cg);
      return;
    case STMT_WHILE:
      mark_helpers_expr(st->v.while_.cond, cg);
      mark_helpers_stmt_list(st->v.while_.body, cg);
      return;
    case STMT_FOREACH:
      cg->used_helpers |= RT_HELPER_AGET;
      mark_helpers_expr(st->v.foreach_.array, cg);
      mark_helpers_stmt_list(st->v.foreach_.body, cg);
      return;
  }
}

static void mark_helpers_stmt_list(const StmtList* list, CodeGen* cg) {
  for (size_t i = 0; i < list->len; i++) {
    mark_helpers_stmt(&list->items[i], cg);
  }
}

static void emit_to_bool(ByteBuf* out, Type ty) {
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

static void emit_bytes_as_byte_directives(ByteBuf* out, const uint8_t* bytes, size_t n) {
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

static bool emit_bytes_as_ascii(ByteBuf* out, const uint8_t* bytes, size_t n) {
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

static void emit_expr_asm(ByteBuf* out, const Expr* e, CodeGen* cg) {
  if (!e) die("internal: null expression");
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
    case EXPR_ARRAY_ALLOC:
      emit_expr_asm(out, e->v.unary.inner, cg);
      fprintf(out, "  CALL __zman_array_alloc\n");
      return;
    case EXPR_INDEX:
      emit_expr_asm(out, e->v.index.base, cg);
      emit_expr_asm(out, e->v.index.index, cg);
      fprintf(out, "  CALL __zman_aget\n");
      return;
    case EXPR_LENGTH:
      emit_expr_asm(out, e->v.unary.inner, cg);
      fprintf(out, "  LOAD32\n");
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

static void emit_stmt_list_asm(ByteBuf* out, const StmtList* list, const Function* cur_fn, CodeGen* cg);

static void emit_stmt_asm(ByteBuf* out, const Stmt* st, const Function* cur_fn, CodeGen* cg) {
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
    case STMT_ASTORE: {
      if (!st->v.astore.target || st->v.astore.target->kind != EXPR_INDEX) {
        die("internal: astore without index target");
      }
      const Expr* t = st->v.astore.target;
      emit_expr_asm(out, t->v.index.base, cg);
      emit_expr_asm(out, t->v.index.index, cg);
      emit_expr_asm(out, st->v.astore.value, cg);
      fprintf(out, "  CALL __zman_aset\n");
      fprintf(out, "  POP\n");
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
    case STMT_FOREACH: {
      uint32_t id = cg->next_label_id++;
      char head_lbl[64];
      char end_lbl[64];
      snprintf(head_lbl, sizeof(head_lbl), "L_foreach_%u_head", (unsigned)id);
      snprintf(end_lbl, sizeof(end_lbl), "L_foreach_%u_end", (unsigned)id);

      // Evaluate array expression once.
      emit_expr_asm(out, st->v.foreach_.array, cg);
      fprintf(out, "  STFP %d\n", st->v.foreach_.arr_slot);

      // idx = 0
      fprintf(out, "  PUSHI 0\n");
      fprintf(out, "  STFP %d\n", st->v.foreach_.idx_slot);

      fprintf(out, "%s:\n", head_lbl);
      // if (idx < length(arr))
      fprintf(out, "  LDFP %d\n", st->v.foreach_.idx_slot);
      fprintf(out, "  LDFP %d\n", st->v.foreach_.arr_slot);
      fprintf(out, "  LOAD32\n");
      fprintf(out, "  LT\n");
      fprintf(out, "  JZ %s\n", end_lbl);

      // var = arr[idx]
      fprintf(out, "  LDFP %d\n", st->v.foreach_.arr_slot);
      fprintf(out, "  LDFP %d\n", st->v.foreach_.idx_slot);
      fprintf(out, "  CALL __zman_aget\n");
      fprintf(out, "  STFP %d\n", st->v.foreach_.var_slot);

      emit_stmt_list_asm(out, st->v.foreach_.body, cur_fn, cg);

      // idx++
      fprintf(out, "  LDFP %d\n", st->v.foreach_.idx_slot);
      fprintf(out, "  PUSHI 1\n");
      fprintf(out, "  ADD\n");
      fprintf(out, "  STFP %d\n", st->v.foreach_.idx_slot);
      fprintf(out, "  JMP %s\n", head_lbl);

      fprintf(out, "%s:\n", end_lbl);
      return;
    }
  }
}

static void emit_stmt_list_asm(ByteBuf* out, const StmtList* list, const Function* cur_fn, CodeGen* cg) {
  for (size_t i = 0; i < list->len; i++) {
    emit_stmt_asm(out, &list->items[i], cur_fn, cg);
  }
}

static void emit_function_asm(ByteBuf* out, const Function* fn, CodeGen* cg) {
  fprintf(out, "%s:\n", fn->label);
  fprintf(out, "  ENTER %d\n", fn->nlocals);
  emit_stmt_list_asm(out, fn->body, fn, cg);
  // Should be unreachable (parser enforces at least one return), but keep a
  // hard runtime failure in case control falls off the end.
  fprintf(out, "  TRAP 1\n");
}

void emit_v0_asm(ByteBuf* out, const StmtList* stmts, const StrPool* sp, const Globals* globals, const FuncTable* funcs) {
  CodeGen cg;
  cg.next_label_id = 0;
  cg.used_helpers = 0;

  // Pre-pass: determine which runtime helpers are actually referenced.
  for (size_t i = 0; i < funcs->len; i++) {
    mark_helpers_stmt_list(funcs->items[i].body, &cg);
  }
  mark_helpers_stmt_list(stmts, &cg);

  fprintf(out, ".module \"zman_program\"\n\n");
  fprintf(out, ".code\n");
  fprintf(out, ".entry main\n\n");

  if (cg.used_helpers & RT_HELPER_PRINT) {
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
  }

  if (cg.used_helpers & RT_HELPER_STRCAT) {
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
  }

  if (cg.used_helpers & RT_HELPER_ARRAY_ALLOC) {
    // __zman_array_alloc(n) -> p
    // Allocates an array object [u32 len][u32 elems[len]] with elements zero-initialized.
    // Traps if n < 0.
    fprintf(out, "__zman_array_alloc:\n");
    fprintf(out, "  ENTER 5\n");
    // store n
    fprintf(out, "  LDFP -3\n");
    fprintf(out, "  STFP 0\n");
    // trap if n < 0
    fprintf(out, "  LDFP 0\n");
    fprintf(out, "  PUSHI 0\n");
    fprintf(out, "  LT\n");
    fprintf(out, "  JZ L_arr_ok\n");
    fprintf(out, "  TRAP 1\n");
    fprintf(out, "L_arr_ok:\n");
    // bytes = 4 + n*4
    fprintf(out, "  LDFP 0\n");
    fprintf(out, "  PUSHI 4\n");
    fprintf(out, "  MUL\n");
    fprintf(out, "  ADDI 4\n");
    fprintf(out, "  STFP 1\n");
    // ptr = heap_alloc(bytes)
    fprintf(out, "  LDFP 1\n");
    fprintf(out, "  SYSCALL 6\n");
    fprintf(out, "  DUP\n");
    fprintf(out, "  STFP 2\n");
    // store len at ptr
    fprintf(out, "  DUP\n");
    fprintf(out, "  LDFP 0\n");
    fprintf(out, "  STORE32\n");
    // elem_ptr = ptr + 4
    fprintf(out, "  DUP\n");
    fprintf(out, "  ADDI 4\n");
    fprintf(out, "  STFP 3\n");
    fprintf(out, "  POP\n");
    // i = 0
    fprintf(out, "  PUSHI 0\n");
    fprintf(out, "  STFP 4\n");
    fprintf(out, "L_arr_init_head:\n");
    fprintf(out, "  LDFP 4\n");
    fprintf(out, "  LDFP 0\n");
    fprintf(out, "  LT\n");
    fprintf(out, "  JZ L_arr_init_end\n");
    // addr = elem_ptr + i*4; *addr = 0
    fprintf(out, "  LDFP 3\n");
    fprintf(out, "  LDFP 4\n");
    fprintf(out, "  PUSHI 4\n");
    fprintf(out, "  MUL\n");
    fprintf(out, "  ADD\n");
    fprintf(out, "  PUSHI 0\n");
    fprintf(out, "  STORE32\n");
    // i++
    fprintf(out, "  LDFP 4\n");
    fprintf(out, "  ADDI 1\n");
    fprintf(out, "  STFP 4\n");
    fprintf(out, "  JMP L_arr_init_head\n");
    fprintf(out, "L_arr_init_end:\n");
    // return ptr
    fprintf(out, "  LDFP 2\n");
    fprintf(out, "  RET 1\n\n");
  }

  if (cg.used_helpers & RT_HELPER_AGET) {
    // __zman_aget(p, i) -> v
    // Traps on i < 0 or i >= len.
    fprintf(out, "__zman_aget:\n");
    fprintf(out, "  ENTER 2\n");
    // idx -> local0
    fprintf(out, "  LDFP -3\n");
    fprintf(out, "  STFP 0\n");
    // if idx < 0 trap
    fprintf(out, "  LDFP 0\n");
    fprintf(out, "  PUSHI 0\n");
    fprintf(out, "  LT\n");
    fprintf(out, "  JZ L_aget_nonneg\n");
    fprintf(out, "  TRAP 1\n");
    fprintf(out, "L_aget_nonneg:\n");
    // keep p on stack; len -> local1
    fprintf(out, "  LDFP -4\n");
    fprintf(out, "  DUP\n");
    fprintf(out, "  LOAD32\n");
    fprintf(out, "  STFP 1\n");
    // if idx >= len trap (i < len must hold)
    fprintf(out, "  LDFP 0\n");
    fprintf(out, "  LDFP 1\n");
    fprintf(out, "  LT\n");
    fprintf(out, "  JNZ L_aget_inrange\n");
    fprintf(out, "  TRAP 1\n");
    fprintf(out, "L_aget_inrange:\n");
    // addr = p + 4 + idx*4
    fprintf(out, "  ADDI 4\n");
    fprintf(out, "  LDFP 0\n");
    fprintf(out, "  PUSHI 4\n");
    fprintf(out, "  MUL\n");
    fprintf(out, "  ADD\n");
    fprintf(out, "  LOAD32\n");
    fprintf(out, "  RET 2\n\n");
  }

  if (cg.used_helpers & RT_HELPER_ASET) {
    // __zman_aset(p, i, v) -> 0
    // Traps on i < 0 or i >= len.
    fprintf(out, "__zman_aset:\n");
    fprintf(out, "  ENTER 2\n");
    // idx -> local0
    fprintf(out, "  LDFP -4\n");
    fprintf(out, "  STFP 0\n");
    // if idx < 0 trap
    fprintf(out, "  LDFP 0\n");
    fprintf(out, "  PUSHI 0\n");
    fprintf(out, "  LT\n");
    fprintf(out, "  JZ L_aset_nonneg\n");
    fprintf(out, "  TRAP 1\n");
    fprintf(out, "L_aset_nonneg:\n");
    // keep p on stack; len -> local1
    fprintf(out, "  LDFP -5\n");
    fprintf(out, "  DUP\n");
    fprintf(out, "  LOAD32\n");
    fprintf(out, "  STFP 1\n");
    // if idx >= len trap (i < len must hold)
    fprintf(out, "  LDFP 0\n");
    fprintf(out, "  LDFP 1\n");
    fprintf(out, "  LT\n");
    fprintf(out, "  JNZ L_aset_inrange\n");
    fprintf(out, "  TRAP 1\n");
    fprintf(out, "L_aset_inrange:\n");
    // addr = p + 4 + idx*4
    fprintf(out, "  ADDI 4\n");
    fprintf(out, "  LDFP 0\n");
    fprintf(out, "  PUSHI 4\n");
    fprintf(out, "  MUL\n");
    fprintf(out, "  ADD\n");
    // store v
    fprintf(out, "  LDFP -3\n");
    fprintf(out, "  STORE32\n");
    fprintf(out, "  PUSHI 0\n");
    fprintf(out, "  RET 3\n\n");
  }

  // user functions
  for (size_t i = 0; i < funcs->len; i++) {
    emit_function_asm(out, &funcs->items[i], &cg);
    fprintf(out, "\n");
  }

  fprintf(out, "main:\n");
  emit_stmt_list_asm(out, stmts, NULL, &cg);
  fprintf(out, "  HALT\n\n");

  fprintf(out, ".data\n");
  fprintf(out, "\n");

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
    fprintf(out, "\n");
  }

  fprintf(out, ".end\n");
}
