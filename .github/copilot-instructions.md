# Copilot instructions for zmanLang / StackVM-32

## Big picture
- This repo is a small C toolchain for a single bytecode format ("ZVM1"):
  - `bin/svm_asm`: assembles `.asm` → `.zvm` container (code + initial memory image).
  - `bin/svm_vm`: runs a `.zvm` container (StackVM-32 interpreter).
  - `bin/svm_disasm`: disassembles `.zvm` and auto-labels branch/call targets.
- Specs are the source of truth for semantics and encoding:
  - VM + container format: `spec-vm.md`
  - Assembler syntax + directives: `spec-assembler.md`
  - High-level language notes / runtime object layouts: `spec-lang.md`

## Critical “keep in sync” areas
- Opcode tables must match across:
  - `src/svm_vm.c` (execution + verifier)
  - `src/svm_disasm.c` (`INSTRS[]` decode table)
  - `src/svm_asm.c` (mnemonic parsing / encoding)
- The ZVM container header layout ("ZVM1", version 1, 28-byte header) is implemented in both:
  - `src/svm_vm.c` (loader)
  - `src/svm_disasm.c` (loader)
  - `src/svm_asm.c` (writer)

## Developer workflows (macOS/Unix)
- Build tools: `make` (default `release`).
  - `make release|debug|asan|ubsan`
  - Outputs go to `bin/` and intermediates to `build/`.
- Run golden tests: `make test` (runs `./run_test.sh`).
  - `run_test.sh` assembles `examples/test{1,2,3}.asm` → `build/*.zvm` and compares stdout against `tests/expected/*.stdout`.
  - The assembler prints a status line to stderr; tests silence it (`2>/dev/null`). Preserve this behavior when changing CLI output.
- Convenience targets:
  - Assemble+run: `make run PROG=examples/test1.asm` (writes `program.zvm` by default).
  - Disassemble: `make disasm ZVM=program.zvm`
  - Static analysis: `make lint` (prefers `clang-tidy`, else `clang --analyze`).

## Project-specific implementation conventions
- Endianness: immediates and LOAD32/STORE32 are little-endian (see `spec-vm.md`; implemented in `src/svm_vm.c`).
- Control-flow targets are absolute byte offsets into `code` (“addr32”), not instruction indices.
- The VM does a one-time code verification pass and records instruction starts; dynamic control flow (e.g., `RET`/`CALLI`) must land on verified boundaries.
- Syscalls are part of the VM ABI (IDs 0–7). Keep any changes consistent with `spec-vm.md` and the comments at the top of `src/svm_vm.c`.

## Web/WASM harness (Emscripten)
- Build: `make web` (runs `examples/web/build_web.sh`, requires `emcc`).
- Harness files:
  - `examples/web/index.html`, `examples/web/harness.js`
  - Emscripten exports come from C APIs in `src/svm_vm.c` and `src/svm_asm.c` (see `examples/web/README.md`).
- When adding/changing exported C functions, update `examples/web/build_web.sh` (`-s EXPORTED_FUNCTIONS=...`).
