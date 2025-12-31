#include "zmc_internal.h"

// Purpose: zmc CLI entrypoint (read source, parse, then emit StackVM-32 assembly).

static void zmc_compile_v0_to_asm_buf(ByteBuf* out, const char* src, size_t src_len) {
  StrPool sp;
  sp_init(&sp);

  Globals globals;
  globals_init(&globals);

  FuncTable funcs;
  ft_init(&funcs);

  ParseCtx ctx;
  ctx.globals = &globals;
  ctx.funcs = &funcs;
  ctx.cur_fn = NULL;

  Parser p;
  parse_init(&p, src, src_len);

  StmtList* main_stmts = stmt_list_new();
  parse_program(&p, &sp, &ctx, main_stmts);
  token_free(&p.cur);

  emit_v0_asm(out, main_stmts, &sp, &globals, &funcs);

  // cleanup
  stmt_list_free(main_stmts);
  globals_free(&globals);
  ft_free(&funcs);
  sp_free(&sp);
}

int main(int argc, char** argv) {
  if (argc != 3) {
    fprintf(stderr, "usage: zmc <input.zm> <output.asm>\n");
    return 2;
  }

  size_t src_len = 0;
  char* src = read_entire_file(argv[1], &src_len);

  FILE* out = fopen(argv[2], "wb");
  if (!out) die("fopen output");

  ByteBuf asm_buf;
  bb_init(&asm_buf);
  zmc_compile_v0_to_asm_buf(&asm_buf, src, src_len);
  if (asm_buf.len) fwrite(asm_buf.data, 1, asm_buf.len, out);

  fclose(out);

  bb_free(&asm_buf);
  free(src);

  return 0;
}

// ------------------------------ Emscripten-friendly single-instance exports ------------------------------
// Mirrors the pattern used by svm_asm: compile from an input buffer, then read output via ptr/len.

static ByteBuf g_zmc_out;
static int g_zmc_out_inited = 0;

int zmc_compile_v0_asm_from_buffer(uint8_t* src, uint32_t len) {
  if (!g_zmc_out_inited) {
    bb_init(&g_zmc_out);
    g_zmc_out_inited = 1;
  }
  g_zmc_out.len = 0;
  zmc_error_clear();

  zmc_trap_set(1);
  if (setjmp(*zmc_trap_jmp()) != 0) {
    zmc_trap_set(0);
    return 0;
  }

  zmc_compile_v0_to_asm_buf(&g_zmc_out, (const char*)src, (size_t)len);

  zmc_trap_set(0);
  return 1;
}

uint32_t zmc_output_ptr(void) {
  if (!g_zmc_out_inited || !g_zmc_out.data) return 0u;
  return (uint32_t)(uintptr_t)g_zmc_out.data;
}

uint32_t zmc_output_len(void) {
  if (!g_zmc_out_inited) return 0u;
  return (uint32_t)g_zmc_out.len;
}

void zmc_output_clear(void) {
  if (!g_zmc_out_inited) return;
  bb_free(&g_zmc_out);
  bb_init(&g_zmc_out);
}
