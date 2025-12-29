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

# Provide a default program for the harness.
cp -f ../../program.zvm ./program.zvm
cp -f ../../examples/test1.asm ./program.asm

echo "built: examples/web/svm_vm.js (+ wasm), examples/web/svm_asm.js (+ wasm)"
echo "copied: examples/web/program.zvm and examples/web/program.asm"
echo "serve:  cd examples/web && python3 -m http.server 8000"
echo "open:   http://localhost:8000"
