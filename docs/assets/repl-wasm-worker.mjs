import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from 'https://unpkg.com/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js';

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
      compiledModule = await WebAssembly.compile(await resp.arrayBuffer());
    }

    // Provide a preopened directory so file-based exports (eval_file_c etc.) work
    const inputFile = new File(new Uint8Array(new TextEncoder().encode(code)));
    const preopen = new PreopenDirectory('.', [['input.mi', inputFile]]);
    const fds = [
      new OpenFile(new File(new Uint8Array([]))),
      ConsoleStdout.lineBuffered((line) => self.postMessage({ type: 'log', msg: line })),
      ConsoleStdout.lineBuffered((line) => self.postMessage({ type: 'log', msg: line })),
      preopen,
    ];

    const wasi = new WASI([], [], fds);
    const instance = await WebAssembly.instantiate(compiledModule, {
      wasi_snapshot_preview1: wasi.wasiImport,
    });

    if (typeof wasi.initialize === 'function') {
      wasi.initialize(instance);
    } else if (instance.exports._initialize) {
      instance.exports._initialize();
    }
    if (instance.exports.hs_init) {
      try { instance.exports.hs_init(0, 0); } catch (_) {}
    }

    const { memory, alloc_bytes, eval_str_c } = instance.exports;
    if (!alloc_bytes || !eval_str_c) {
      throw new Error('WASM module missing eval_str_c/alloc_bytes exports');
    }

    // Write code into WASM memory and call eval_str_c
    const encoded = new TextEncoder().encode(code);
    const bufPtr = Number(alloc_bytes(encoded.length + 1));
    const mem = new Uint8Array(memory.buffer);
    mem.set(encoded, bufPtr);
    mem[bufPtr + encoded.length] = 0;

    const resPtr = Number(eval_str_c(bufPtr));
    // Read null-terminated result string
    const mem2 = new Uint8Array(memory.buffer);
    let i = resPtr;
    const out = [];
    while (i < mem2.length && mem2[i] !== 0) { out.push(mem2[i]); i++; }
    const result = new TextDecoder().decode(new Uint8Array(out));

    if (result.startsWith('ERR:')) {
      self.postMessage({ type: 'error', error: result.slice(4) });
    } else {
      self.postMessage({ type: 'result', result });
    }
  } catch (err) {
    self.postMessage({ type: 'error', error: String(err) });
  }
});
