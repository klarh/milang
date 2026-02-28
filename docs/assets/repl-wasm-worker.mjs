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

    const stdinFile = new File(new Uint8Array(new TextEncoder().encode(code)));
    const fds = [
      new OpenFile(stdinFile),
      ConsoleStdout.lineBuffered((msg) => self.postMessage({ type: 'log', msg })),
      ConsoleStdout.lineBuffered((msg) => self.postMessage({ type: 'log', msg })),
      new PreopenDirectory('.', []),
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

    if (instance.exports && instance.exports._start) {
      try {
        instance.exports._start();
      } catch (err) {
        // Some runtimes use exceptions for process exit - ignore
      }
    }

    self.postMessage({ type: 'result', result: 'OK' });
  } catch (err) {
    self.postMessage({ type: 'error', error: String(err) });
  }
});
