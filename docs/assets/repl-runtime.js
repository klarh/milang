(() => {
  const outputEl = document.getElementById('output');
  const editor = document.getElementById('editor');
  const runBtn = document.getElementById('run');
  const stopBtn = document.getElementById('stop');
  let worker = null;
  let timeoutId = null;
  function log(msg) { outputEl.textContent += msg + '\n'; }
  function resetOutput(){ outputEl.textContent = ''; }

  async function createWorker() {
    if (worker) { worker.terminate(); worker = null; }
    const wasmURL = '/assets/wasm/milang-wasm.wasm';
    let haveWasm = false;
    try {
      if (location.protocol !== 'file:') {
        const res = await fetch(wasmURL, { method: 'HEAD' });
        haveWasm = res.ok;
      }
    } catch (e) { haveWasm = false; }

    if (!haveWasm || typeof Worker === 'undefined') {
      log('WASM unavailable — sandbox requires a web server with WASM support.');
      return;
    }

    try {
      worker = new Worker('./assets/repl-wasm-worker.mjs', { type: 'module' });
    } catch (err) {
      log('Failed to start worker: ' + err);
      return;
    }

    worker.onmessage = (e) => {
      const data = e.data;
      if (data.type === 'log') { log(String(data.msg)); }
      else if (data.type === 'result') { log('=> ' + data.result); clearTimeout(timeoutId); }
      else if (data.type === 'error') { log('Error: ' + data.error); clearTimeout(timeoutId); }
    };
    worker.onerror = (e) => {
      log('Worker error: ' + e.message);
      clearTimeout(timeoutId);
    };
  }

  runBtn.addEventListener('click', async () => {
    resetOutput();
    await createWorker();
    if (!worker) { return; }
    const code = editor.value;
    const wasmURL = '/assets/wasm/milang-wasm.wasm';
    worker.postMessage({ type: 'run', code, wasmURL });
    timeoutId = setTimeout(() => {
      if (worker) { worker.terminate(); worker = null; }
      log('Terminated: timeout (5s)');
    }, 5000);
  });

  stopBtn.addEventListener('click', () => {
    clearTimeout(timeoutId);
    if (worker) { worker.terminate(); worker = null; log('Stopped.'); }
  });
})();
