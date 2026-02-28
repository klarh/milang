import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from 'https://unpkg.com/@bjorn3/browser_wasi_shim/dist/index.js';

let compiledModule = null;

self.addEventListener('message', async (e) => {
  const msg = e.data;
  if (msg.type !== 'run') return;
  const code = msg.code || '';
  const wasmURL = msg.wasmURL || './assets/wasm/milang-wasm.wasm';
  try {
    if (!compiledModule) {
      const resp = await fetch(wasmURL);
      if (!resp.ok) throw new Error('Failed to fetch wasm: ' + resp.status);
      const wasmArray = await resp.arrayBuffer();
      compiledModule = await WebAssembly.compile(wasmArray);
    }

    // Create a preopened directory with a file 'input.mi' containing the code
    const inputFile = new File(new Uint8Array(new TextEncoder().encode(code)));
    const preopen = new PreopenDirectory('.', [[ 'input.mi', inputFile ]]);
    const fds = [
      new OpenFile(new File(new Uint8Array([]))),
      ConsoleStdout.lineBuffered((msg) => self.postMessage({ type: 'log', msg })),
      ConsoleStdout.lineBuffered((msg) => self.postMessage({ type: 'log', msg })),
      preopen,
    ];

    const wasi = new WASI([], {}, fds);
    const instance = await WebAssembly.instantiate(compiledModule, {
      wasi_snapshot_preview1: wasi.wasiImport,
    });

    // initialize if present
    if (typeof wasi.initialize === 'function') {
      wasi.initialize(instance);
    } else if (instance.exports && instance.exports._initialize) {
      instance.exports._initialize();
    }

    if (instance.exports && instance.exports.hs_init) {
      try { instance.exports.hs_init(0, 0); } catch (e) { /* ignore */ }
    }

    // Call the exported eval_file_c function if available
    if (instance.exports && instance.exports.eval_file_c) {
      try {
        const resPtr = instance.exports.eval_file_c();
        // read null-terminated C string from memory
        const mem = new Uint8Array(instance.exports.memory.buffer);
        let i = resPtr;
        let bytes = [];
        while (mem[i] !== 0) { bytes.push(mem[i]); i++; }
        const resultStr = new TextDecoder().decode(new Uint8Array(bytes));
        self.postMessage({ type: 'result', result: resultStr });
      } catch (err) {
        self.postMessage({ type: 'error', error: String(err) });
      }
    } else {
      // fallback: call _start if present
      if (instance.exports && instance.exports._start) {
        try {
          instance.exports._start();
        } catch (err) { /* ignore */ }
      }
      self.postMessage({ type: 'result', result: 'OK' });
    }
  } catch (err) {
    self.postMessage({ type: 'error', error: String(err) });
  }
});
