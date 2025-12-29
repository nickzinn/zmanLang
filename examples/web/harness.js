/* global createSvmVm */

(async function main() {
  const statusEl = document.getElementById('status');
  const outEl = document.getElementById('out');
  const runBtn = document.getElementById('runBtn');

  function setStatus(s) {
    statusEl.textContent = s;
  }

  function appendOut(text) {
    outEl.textContent += text;
  }

  function clearOut() {
    outEl.textContent = '';
  }

  let Module;

  async function ensureModule() {
    if (Module) return Module;
    setStatus('Loading wasm module…');
    Module = await createSvmVm();
    setStatus('Ready');
    return Module;
  }

  async function runProgram() {
    clearOut();
    const M = await ensureModule();

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

  runBtn.addEventListener('click', () => {
    runProgram().catch((err) => {
      setStatus('Error');
      clearOut();
      appendOut(String(err && err.stack ? err.stack : err));
    });
  });

  // Auto-initialize module (but do not auto-run).
  ensureModule().catch((err) => {
    setStatus('Error loading module');
    appendOut(String(err && err.stack ? err.stack : err));
  });
})();
