# Copilot instructions for zmanLang (StackVM-32 toolchain)

## Big picture (what talks to what)
- Toolchain flow: `zmc` (ZmanLang `.zm` → StackVM-32 `.asm`) → `svm_asm` (`.asm` → ZVM1 `.zvm`) → `svm_vm` (executes `.zvm`). `svm_disasm` goes `.zvm` → readable `.asm` with auto-labels.
- Specs are the source of truth: `spec-lang.md` (language/runtime), `spec-assembler.md` (syntax/directives), `spec-vm.md` (ISA + ZVM1 container).

## Critical “keep in sync” areas
- Opcode/encoding tables must match across `src/svm_vm.c` (exec + verifier), `src/svm_asm.c` (mnemonics/encoding), `src/svm_disasm.c` (`INSTRS[]` decode).
- ZVM1 header layout (magic "ZVM1", version 1, 28-byte header) is implemented in `src/svm_asm.c` (writer) and `src/svm_vm.c` / `src/svm_disasm.c` (loaders).
- Syscall ABI is part of the VM contract (currently IDs 0–9; see `spec-vm.md` and the header comment in `src/svm_vm.c`). `zmc` emits `SYSCALL 8/9` for `text()`/`number()` (see `src/zmc_codegen.c`).

## Workflows (macOS/Unix)
- Build: `make release|debug|asan|ubsan` → binaries in `bin/`, objects in `build/`.
- Golden tests: `make test` (runs `run_test.sh`). Note: tests silence the assembler’s status line by redirecting stderr (`./bin/svm_asm ... 2>/dev/null`); keep stderr behavior stable.
- Handy targets: `make run PROG=examples/test1.asm` (writes `program.zvm`), `make disasm ZVM=program.zvm`, `make lint`.

## Project conventions / sharp edges
- Endianness: immediates and `LOAD32`/`STORE32` are little-endian (`spec-vm.md`, implemented in `src/svm_vm.c`).
- Control-flow operands (`addr32`) are absolute *byte offsets* into the code blob (not instruction indices).
- The VM performs a one-time verification pass and records instruction starts; dynamic control-flow (e.g. `RET`, `CALLI`) must land on verified boundaries.
- `zmc` pipeline is “parse → AST/IR → emit asm”; see `src/zmc_main.c` + `src/zmc_parser.c` + `src/zmc_codegen.c`.
- `zmc_codegen.c` emits via `ByteBuf` and intentionally remaps `fprintf/fputs/fputc` to buffer writers; keep that pattern when touching codegen.

## Web/WASM harness (Emscripten)
- `make web` runs `examples/web/build_web.sh`. Browser harness is `examples/web/index.html` + `examples/web/harness.js`.
- WASM APIs are ptr/len-style exports (documented in `examples/web/README.md`). When adding/changing exports, update `examples/web/build_web.sh` (`-s EXPORTED_FUNCTIONS=...`).
