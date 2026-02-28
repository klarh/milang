(() => {
  const outputEl = document.getElementById('output');
  const editor = document.getElementById('editor');
  const runBtn = document.getElementById('run');
  const stopBtn = document.getElementById('stop');
  let worker = null;
  let timeoutId = null;
  function log(msg) { outputEl.textContent += msg + '\n'; }
  function resetOutput(){ outputEl.textContent = ''; }
  function startWorker() {
    if (worker) worker.terminate();
    worker = new Worker('./assets/repl-worker.js');
    worker.onmessage = (e) => {
      const data = e.data;
      if (data.type === 'log') { log(String(data.msg)); }
      else if (data.type === 'result') { log('=> ' + data.result); clearTimeout(timeoutId); }
      else if (data.type === 'error') { log('Error: ' + data.error); clearTimeout(timeoutId); }
    };
  }
  runBtn.addEventListener('click', () => {
    resetOutput();
    startWorker();
    const code = editor.value;
    worker.postMessage({ type: 'run', code, steps: 200000 });
    timeoutId = setTimeout(() => {
      worker.terminate();
      log('Terminated: timeout');
      worker = null;
    }, 2000);
  });
  stopBtn.addEventListener('click', () => {
    if (worker) { worker.terminate(); worker = null; log('Stopped by user'); }
  });
  // start initial worker
  startWorker();
})();
