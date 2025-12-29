/* global createSvmVm, createSvmAsm */

(async function main() {
  const statusEl = document.getElementById('status');
  const outEl = document.getElementById('out');
  const runBtn = document.getElementById('runBtn');
  const asmRunBtn = document.getElementById('asmRunBtn');
  const asmSrcEl = document.getElementById('asmSrc');

  function setStatus(s) {
    statusEl.textContent = s;
  }

  function appendOut(text) {
    outEl.textContent += text;
  }

  function clearOut() {
    outEl.textContent = '';
  }

  function showError(err) {
    setStatus('Error');
    clearOut();
    if (err && typeof err === 'object' && err.isAsmError && typeof err.message === 'string') {
      appendOut(err.message);
      return;
    }
    if (err && typeof err === 'object' && typeof err.stack === 'string') {
      appendOut(err.stack);
      return;
    }
    if (err && typeof err === 'object' && typeof err.message === 'string') {
      appendOut(err.message);
      return;
    }
    appendOut(String(err));
  }

  let Vm;
  let Asm;

  async function ensureVm() {
    if (Vm) return Vm;
    setStatus('Loading VM wasm module…');
    Vm = await createSvmVm();
    setStatus('Ready');
    return Vm;
  }

  async function ensureAsm() {
    if (Asm) return Asm;
    setStatus('Loading assembler wasm module…');
    Asm = await createSvmAsm();
    setStatus('Ready');
    return Asm;
  }

  async function runProgram() {
    clearOut();
    const M = await ensureVm();

    setStatus('Fetching program.zvm…');
    const resp = await fetch('./program.zvm');
    if (!resp.ok) {
      throw new Error(`fetch program.zvm failed: ${resp.status} ${resp.statusText}`);
    }
    const bytes = new Uint8Array(await resp.arrayBuffer());

    // Copy container bytes into WASM memory so the VM can point into it.
    const ptr = M._malloc(bytes.length);
    if (!ptr) throw new Error('malloc failed');
    M.HEAPU8.set(bytes, ptr);

    const stackWords = 65536;
    const ok = M._svm_vm_load_from_buffer(ptr, bytes.length, stackWords);
    if (!ok) {
      M._free(ptr);
      throw new Error('svm_vm_load_from_buffer failed (invalid container?)');
    }

    setStatus('Running…');
    const rc = M._svm_vm_run_loaded();

    const outPtr = M._svm_vm_output_ptr();
    const outLen = M._svm_vm_output_len();

    if (outPtr && outLen) {
      const outBytes = M.HEAPU8.slice(outPtr, outPtr + outLen);
      const text = new TextDecoder('utf-8').decode(outBytes);
      appendOut(text);
    }

    if (rc === -1) {
      const trap = M._svm_vm_trap_code();
      const ip = M._svm_vm_ip();
      setStatus(`TRAP: code=0x${trap.toString(16).padStart(8, '0')} ip=0x${ip.toString(16).padStart(8, '0')}`);
    } else {
      setStatus(`Exited: rc=${rc}`);
    }

    M._svm_vm_free_loaded();
    M._free(ptr);
  }

  async function assembleAndRun() {
    clearOut();
    const A = await ensureAsm();
    const V = await ensureVm();

    const asmText = String(asmSrcEl && asmSrcEl.value ? asmSrcEl.value : '');
    if (!asmText.trim()) {
      throw new Error('assembly source is empty');
    }
    const asmBytes = new TextEncoder().encode(asmText);

    // Copy assembly source bytes into assembler WASM memory.
    const srcPtr = A._malloc(asmBytes.length);
    if (!srcPtr) throw new Error('asm malloc failed');
    A.HEAPU8.set(asmBytes, srcPtr);

    setStatus('Assembling…');
    const ok = A._svm_asm_assemble_from_buffer(srcPtr, asmBytes.length);
    A._free(srcPtr);

    if (!ok) {
      const errPtr = A._svm_asm_error_ptr();
      const errLen = A._svm_asm_error_len();
      if (errPtr && errLen) {
        const errBytes = A.HEAPU8.slice(errPtr, errPtr + errLen);
        const errText = new TextDecoder('utf-8').decode(errBytes);
        const e = new Error(errText);
        e.isAsmError = true;
        throw e;
      }
      const e = new Error('assembly failed');
      e.isAsmError = true;
      throw e;
    }

    const outPtr = A._svm_asm_output_ptr();
    const outLen = A._svm_asm_output_len();
    if (!outPtr || !outLen) throw new Error('assembler produced empty output');
    const zvmBytes = A.HEAPU8.slice(outPtr, outPtr + outLen);

    // Copy container bytes into VM WASM memory.
    const vmPtr = V._malloc(zvmBytes.length);
    if (!vmPtr) throw new Error('vm malloc failed');
    V.HEAPU8.set(zvmBytes, vmPtr);

    const stackWords = 65536;
    const loaded = V._svm_vm_load_from_buffer(vmPtr, zvmBytes.length, stackWords);
    if (!loaded) {
      V._free(vmPtr);
      throw new Error('svm_vm_load_from_buffer failed (invalid container?)');
    }

    setStatus('Running…');
    const rc = V._svm_vm_run_loaded();

    const outPtr2 = V._svm_vm_output_ptr();
    const outLen2 = V._svm_vm_output_len();
    if (outPtr2 && outLen2) {
      const outBytes = V.HEAPU8.slice(outPtr2, outPtr2 + outLen2);
      const text = new TextDecoder('utf-8').decode(outBytes);
      appendOut(text);
    }

    if (rc === -1) {
      const trap = V._svm_vm_trap_code();
      const ip = V._svm_vm_ip();
      setStatus(`TRAP: code=0x${trap.toString(16).padStart(8, '0')} ip=0x${ip.toString(16).padStart(8, '0')}`);
    } else {
      setStatus(`Exited: rc=${rc}`);
    }

    V._svm_vm_free_loaded();
    V._free(vmPtr);
  }

  runBtn.addEventListener('click', () => {
    runProgram().catch((err) => {
      showError(err);
    });
  });

  asmRunBtn.addEventListener('click', () => {
    assembleAndRun().catch((err) => {
      showError(err);
    });
  });

  // Auto-initialize modules (but do not auto-run).
  Promise.all([ensureVm(), ensureAsm()])
    .then(async () => {
      // Best-effort: preload the example into the textarea.
      if (asmSrcEl && !asmSrcEl.value) {
        try {
          const resp = await fetch('./program.asm');
          if (resp.ok) asmSrcEl.value = await resp.text();
        } catch {
          // ignore
        }
      }
    })
    .catch((err) => {
      setStatus('Error loading module');
      appendOut(String(err && err.stack ? err.stack : err));
    });
})();
