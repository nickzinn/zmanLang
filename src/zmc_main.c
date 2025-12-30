#include "zmc_internal.h"

// Purpose: zmc CLI entrypoint (read source, parse, then emit StackVM-32 assembly).

int main(int argc, char** argv) {
  if (argc != 3) {
    fprintf(stderr, "usage: zmc <input.zm> <output.asm>\n");
    return 2;
  }

  size_t src_len = 0;
  char* src = read_entire_file(argv[1], &src_len);

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

  FILE* out = fopen(argv[2], "wb");
  if (!out) die("fopen output");

  emit_v0_asm(out, main_stmts, &sp, &globals, &funcs);

  fclose(out);

  // cleanup
  stmt_list_free(main_stmts);
  globals_free(&globals);
  ft_free(&funcs);
  sp_free(&sp);
  free(src);

  return 0;
}
