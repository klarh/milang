(() => {
  const outputEl = document.getElementById('output');
  const editor = document.getElementById('editor');
  const runBtn = document.getElementById('run');
  const stopBtn = document.getElementById('stop');
  let worker = null;
  let timeoutId = null;
  function log(msg) { outputEl.textContent += msg + '\n'; }
  function resetOutput(){ outputEl.textContent = ''; }

  // JS fallback worker source (classic) — evaluates a tiny Lisp-like language when wasm unavailable
  const fallbackWorkerSource = `self.addEventListener('message', (e) => {\n  const msg = e.data;\n  if (msg.type === 'run') {\n    const code = msg.code || '';\n    const steps = typeof msg.steps === 'number' ? msg.steps : 100000;\n    try {\n      const ast = parse(code);\n      const result = evaluate(ast, standardEnv(), { steps });\n      self.postMessage({ type: 'result', result: serialize(result) });\n    } catch (err) {\n      self.postMessage({ type: 'error', error: String(err) });\n    }\n  }\n});\n\n${/* tokenizer, parse, env, eval functions from previous worker */''}\n\nfunction tokenize(input) {\n  const re = /\\s*([()]|;.*|[^\\s()]+)/g;\n  const tokens = [];\n  let m;\n  while ((m = re.exec(input)) !== null) {\n    const t = m[1];\n    if (t[0] === ';') continue;\n    tokens.push(t);\n  }\n  return tokens;\n}\n\nfunction parse(input) {\n  const tokens = tokenize(input);\n  let i = 0;\n  function parseExpr() {\n    if (i >= tokens.length) throw new Error('Unexpected EOF');\n    const tok = tokens[i++];\n    if (tok === '(') {\n      const list = [];\n      while (tokens[i] !== ')' && i < tokens.length) { list.push(parseExpr()); }\n      if (tokens[i] !== ')') throw new Error('Expected )');\n      i++;\n      return { type: 'list', elements: list };\n    } else if (tok === ')') { throw new Error('Unexpected )'); } else { const num = Number(tok); if (!Number.isNaN(num)) return { type: 'number', value: num }; return { type: 'symbol', name: tok }; }\n  }\n  const exprs = []; while (i < tokens.length) exprs.push(parseExpr());\n  return exprs.length === 1 ? exprs[0] : { type: 'list', elements: [ { type:'symbol', name: 'begin' }, ...exprs ] };\n}\n\nfunction standardEnv() {\n  const env = Object.create(null);\n  env['+'] = (a,b) => a + b;\n  env['-'] = (a,b) => a - b;\n  env['*'] = (a,b) => a * b;\n  env['/'] = (a,b) => a / b;\n  env['print'] = (...args) => { self.postMessage({ type: 'log', msg: args.join(' ') }); return null; };\n  env['>'] = (a,b) => a > b;\n  env['<'] = (a,b) => a < b;\n  env['='] = (a,b) => a === b;\n  return env;\n}\n\nfunction serialize(x) { if (x === null || x === undefined) return 'null'; if (typeof x === 'object') return JSON.stringify(x); return String(x); }\n\nfunction evaluate(node, env, opts) { const steps = { val: opts.steps || 100000 }; function _eval(n, e) { if (steps.val-- <= 0) throw new Error('step limit exceeded'); if (n.type === 'number') return n.value; if (n.type === 'symbol') { if (n.name in e) return e[n.name]; let cur = e.__parent; while (cur) { if (n.name in cur) return cur[n.name]; cur = cur.__parent; } throw new Error('Unbound symbol: ' + n.name); } if (n.type === 'list') { if (n.elements.length === 0) return null; const first = n.elements[0]; if (first.type === 'symbol') { const name = first.name; if (name === 'if') { const cond = _eval(n.elements[1], e); return cond ? _eval(n.elements[2], e) : _eval(n.elements[3], e); } if (name === 'lambda') { const params = n.elements[1].elements.map(p => p.name); const body = n.elements[2]; return { type: 'closure', params, body, env: e }; } if (name === 'define') { const sym = n.elements[1].name; const val = _eval(n.elements[2], e); e[sym] = val; return val; } if (name === 'begin') { let res = null; for (let i = 1; i < n.elements.length; i++) res = _eval(n.elements[i], e); return res; } } const fn = _eval(first, e); if (typeof fn === 'function') { const args = []; for (let i = 1; i < n.elements.length; i++) args.push(_eval(n.elements[i], e)); return fn(...args); } else if (fn && fn.type === 'closure') { const newEnv = Object.create(null); for (let i = 0; i < fn.params.length; i++) { newEnv[fn.params[i]] = _eval(n.elements[i+1], e); } newEnv.__parent = fn.env; return _eval(fn.body, newEnv); } else { throw new Error('Not a function'); } } throw new Error('Unknown node type: ' + n.type); } return _eval(node, env); }
`;

  async function createWorker() {
    if (worker) worker.terminate();
    const wasmURL = '/assets/wasm/milang-wasm.wasm';
    let haveWasm = false;
    try {
      if (location.protocol !== 'file:') {
        const res = await fetch(wasmURL, { method: 'HEAD' });
        haveWasm = res.ok;
      }
    } catch (e) { haveWasm = false; }

    if (haveWasm && typeof Worker === 'function') {
      try {
        worker = new Worker('./assets/repl-wasm-worker.mjs', { type: 'module' });
      } catch (err) {
        log('Failed to start module worker: ' + err);
        worker = null;
      }
    }

    if (!worker) {
      // No JS fallback provided; require the wasm-backed worker for correct Milang evaluation
      log('WASM worker unavailable and no JS fallback is provided; REPL requires wasm.');
      worker = null;
    }

    worker.onmessage = (e) => {
      const data = e.data;
      if (data.type === 'log') { log(String(data.msg)); }
      else if (data.type === 'result') { log('=> ' + data.result); clearTimeout(timeoutId); }
      else if (data.type === 'error') { log('Error: ' + data.error); clearTimeout(timeoutId); }
    };
  }

  runBtn.addEventListener('click', async () => {
    resetOutput();
    await createWorker();
    if (!worker) { log('Worker unavailable'); return; }
    const code = editor.value;
    worker.postMessage({ type: 'run', code, wasmURL: '/assets/wasm/milang-wasm.wasm' });
    timeoutId = setTimeout(() => { if (worker) { worker.terminate(); worker = null; } log('Terminated: timeout'); }, 5000);
  });

  stopBtn.addEventListener('click', () => {
    if (worker) { worker.terminate(); worker = null; log('Stopped by user'); }
  });

  // start initial worker
  createWorker();
})();
