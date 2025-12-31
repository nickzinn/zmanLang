/* global createSvmVm, createSvmAsm, createZmc */

(async function main() {
  const statusEl = document.getElementById('status');
  const outEl = document.getElementById('out');
  const asmRunBtn = document.getElementById('asmRunBtn');
  const zmRunBtn = document.getElementById('zmRunBtn');
  const asmSrcEl = document.getElementById('asmSrc');
  const zmSrcEl = document.getElementById('zmSrc');

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
    if (err && typeof err === 'object' && (err.isAsmError || err.isZmcError) && typeof err.message === 'string') {
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
  let Zmc;

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

  async function ensureZmc() {
    if (Zmc) return Zmc;
    setStatus('Loading compiler wasm module…');
    Zmc = await createZmc();
    setStatus('Ready');
    return Zmc;
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

  async function compileZmToAsm() {
    const C = await ensureZmc();

    const zmText = String(zmSrcEl && zmSrcEl.value ? zmSrcEl.value : '');
    if (!zmText.trim()) {
      throw new Error('source is empty');
    }

    const srcBytes = new TextEncoder().encode(zmText);
    const srcPtr = C._malloc(srcBytes.length);
    if (!srcPtr) throw new Error('zmc malloc failed');
    C.HEAPU8.set(srcBytes, srcPtr);

    setStatus('Compiling…');
    const ok = C._zmc_compile_v0_asm_from_buffer(srcPtr, srcBytes.length);
    C._free(srcPtr);

    if (!ok) {
      const errPtr = C._zmc_error_ptr();
      const errLen = C._zmc_error_len();
      if (errPtr && errLen) {
        const errBytes = C.HEAPU8.slice(errPtr, errPtr + errLen);
        const errText = new TextDecoder('utf-8').decode(errBytes);
        const e = new Error(errText);
        e.isZmcError = true;
        throw e;
      }
      const e = new Error('compile failed');
      e.isZmcError = true;
      throw e;
    }

    const outPtr = C._zmc_output_ptr();
    const outLen = C._zmc_output_len();
    if (!outPtr || !outLen) throw new Error('compiler produced empty output');
    const outBytes = C.HEAPU8.slice(outPtr, outPtr + outLen);
    return new TextDecoder('utf-8').decode(outBytes);
  }

  async function compileAssembleAndRunZm() {
    clearOut();
    const asmText = await compileZmToAsm();
    if (asmSrcEl) asmSrcEl.value = asmText;
    await assembleAndRun();
  }

  asmRunBtn.addEventListener('click', () => {
    assembleAndRun().catch((err) => {
      showError(err);
    });
  });

  zmRunBtn.addEventListener('click', () => {
    compileAssembleAndRunZm().catch((err) => {
      showError(err);
    });
  });

  // Auto-initialize modules (but do not auto-run).
  Promise.all([ensureVm(), ensureAsm(), ensureZmc()])
    .then(async () => {
      if (zmSrcEl && !zmSrcEl.value) {
        try {
          const resp = await fetch('./program.zm');
          if (resp.ok) zmSrcEl.value = await resp.text();
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
