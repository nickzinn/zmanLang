# Web (Emscripten) harness

This folder is a minimal browser harness for running the StackVM-32 interpreter compiled to WebAssembly.

## Build

1. Activate Emscripten (`emcc` must be on PATH).
2. Build the wasm bundle:

- `./build_web.sh`

This generates:
- `svm_vm.js`
- `svm_vm.wasm`
- `svm_asm.js`
- `svm_asm.wasm`
- `program.zvm` (copied from repo root)
- `program.asm` (copied from `examples/test1.asm`)

## Run

Browsers require serving files over HTTP (not `file://`). From this folder:

- `python3 -m http.server 8000`

Then open:

- `http://localhost:8000`

Click **Run program.zvm** and the VM output should appear in the page.

Click **Assemble+Run program.asm** to assemble in the browser (no file I/O) from the textarea contents (prefilled from `program.asm`), then execute the resulting container via the VM module.

## Notes

- The harness uses the exported C API from [src/svm_vm.c](src/svm_vm.c):
  - `svm_vm_load_from_buffer(ptr,len,stackWords)`
  - `svm_vm_run_loaded()`
  - `svm_vm_output_ptr()` / `svm_vm_output_len()`
  - `svm_vm_free_loaded()`

- The harness uses the exported C API from `src/svm_asm.c`:
  - `svm_asm_assemble_from_buffer(ptr,len)`
  - `svm_asm_output_ptr()` / `svm_asm_output_len()`
  - `svm_asm_error_ptr()` / `svm_asm_error_len()`

- Output is captured inside the VM (no stdio) when compiled under `__EMSCRIPTEN__`.
