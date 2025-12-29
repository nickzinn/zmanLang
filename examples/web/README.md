# Web (Emscripten) harness

This folder is a minimal browser harness for running the StackVM-32 interpreter compiled to WebAssembly.

## Build

1. Activate Emscripten (`emcc` must be on PATH).
2. Build the wasm bundle:

- `./build_web.sh`

This generates:
- `svm_vm.js`
- `svm_vm.wasm`
- `program.zvm` (copied from repo root)

## Run

Browsers require serving files over HTTP (not `file://`). From this folder:

- `python3 -m http.server 8000`

Then open:

- `http://localhost:8000`

Click **Run program.zvm** and the VM output should appear in the page.

## Notes

- The harness uses the exported C API from [src/svm_vm.c](src/svm_vm.c):
  - `svm_vm_load_from_buffer(ptr,len,stackWords)`
  - `svm_vm_run_loaded()`
  - `svm_vm_output_ptr()` / `svm_vm_output_len()`
  - `svm_vm_free_loaded()`

- Output is captured inside the VM (no stdio) when compiled under `__EMSCRIPTEN__`.
