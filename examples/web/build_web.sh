#!/usr/bin/env bash
set -euo pipefail

# Builds the StackVM-32 interpreter to WebAssembly via Emscripten.
# Output: examples/web/svm_vm.js + examples/web/svm_vm.wasm
# Also copies ../../program.zvm into examples/web/program.zvm for the harness.

cd "$(dirname "$0")"

if ! command -v emcc >/dev/null 2>&1; then
  echo "error: emcc not found. Activate Emscripten SDK (emsdk_env.sh) first." >&2
  exit 1
fi

# Build the VM.
emcc ../../src/svm_vm.c \
  -O3 \
  -s WASM=1 \
  -s ENVIRONMENT=web \
  -s MODULARIZE=1 \
  -s EXPORT_NAME='createSvmVm' \
  -s ALLOW_MEMORY_GROWTH=1 \
  -s EXPORTED_FUNCTIONS='[
    "_svm_vm_load_from_buffer",
    "_svm_vm_run_loaded",
    "_svm_vm_output_ptr",
    "_svm_vm_output_len",
    "_svm_vm_output_clear",
    "_svm_vm_trap_code",
    "_svm_vm_ip",
    "_svm_vm_free_loaded",
    "_malloc",
    "_free"
  ]' \
  -s EXPORTED_RUNTIME_METHODS='["HEAPU8"]' \
  -o svm_vm.js

# Build the assembler.
emcc ../../src/svm_asm.c \
  -O3 \
  -s WASM=1 \
  -s ENVIRONMENT=web \
  -s MODULARIZE=1 \
  -s EXPORT_NAME='createSvmAsm' \
  -s ALLOW_MEMORY_GROWTH=1 \
  -s EXPORTED_FUNCTIONS='[
    "_svm_asm_assemble_from_buffer",
    "_svm_asm_output_ptr",
    "_svm_asm_output_len",
    "_svm_asm_output_clear",
    "_svm_asm_error_ptr",
    "_svm_asm_error_len",
    "_svm_asm_error_clear",
    "_malloc",
    "_free"
  ]' \
  -s EXPORTED_RUNTIME_METHODS='["HEAPU8"]' \
  -o svm_asm.js

# Build the compiler.
emcc \
  ../../src/zmc_main.c \
  ../../src/zmc_codegen.c \
  ../../src/zmc_ir.c \
  ../../src/zmc_lexer.c \
  ../../src/zmc_parser.c \
  ../../src/zmc_util.c \
  -O3 \
  -s WASM=1 \
  -s ENVIRONMENT=web \
  -s MODULARIZE=1 \
  -s EXPORT_NAME='createZmc' \
  -s ALLOW_MEMORY_GROWTH=1 \
  -s EXPORTED_FUNCTIONS='[
    "_zmc_compile_v0_asm_from_buffer",
    "_zmc_output_ptr",
    "_zmc_output_len",
    "_zmc_output_clear",
    "_zmc_error_ptr",
    "_zmc_error_len",
    "_zmc_error_clear",
    "_malloc",
    "_free"
  ]' \
  -s EXPORTED_RUNTIME_METHODS='["HEAPU8"]' \
  -o zmc.js

# Provide a default program for the harness.
cp -f ../../program.zvm ./program.zvm
cp -f ../../examples/test1.asm ./program.asm
cp -f ../../examples/web_sample.zm ./program.zm

echo "built: examples/web/svm_vm.js (+ wasm), examples/web/svm_asm.js (+ wasm), examples/web/zmc.js (+ wasm)"
echo "copied: examples/web/program.zvm and examples/web/program.asm"
echo "serve:  cd examples/web && python3 -m http.server 8000"
echo "open:   http://localhost:8000"
