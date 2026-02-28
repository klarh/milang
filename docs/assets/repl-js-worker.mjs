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

    // Provide input.mi via a preopened directory so Haskell can read it
    const inputFile = new File(new Uint8Array(new TextEncoder().encode(code)));
    const preopen = new PreopenDirectory('.', [[ 'input.mi', inputFile ]]);
    const fds = [
      new OpenFile(new File(new Uint8Array([]))),
      ConsoleStdout.lineBuffered((msg) => self.postMessage({ type: 'log', msg })),
      ConsoleStdout.lineBuffered((msg) => self.postMessage({ type: 'log', msg })),
      preopen,
    ];

    const wasi = new WASI([], [], fds);
    const instance = await WebAssembly.instantiate(compiledModule, {
      wasi_snapshot_preview1: wasi.wasiImport,
    });

    if (typeof wasi.initialize === 'function') {
      wasi.initialize(instance);
    } else if (instance.exports && instance.exports._initialize) {
      instance.exports._initialize();
    }

    if (instance.exports && instance.exports.hs_init) {
      try { instance.exports.hs_init(0, 0); } catch (e) { /* ignore */ }
    }

    // Call parse_file_c to get JSON AST
    if (!(instance.exports && instance.exports.parse_file_c)) {
      throw new Error('WASM module does not export parse_file_c');
    }

    try {
      const resPtr = instance.exports.parse_file_c();
      const mem = new Uint8Array(instance.exports.memory.buffer);
      let i = resPtr;
      const bytes = [];
      while (mem[i] !== 0) { bytes.push(mem[i]); i++; }
      const astJson = new TextDecoder().decode(new Uint8Array(bytes));

      if (astJson.startsWith('ERR:')) {
        self.postMessage({ type: 'error', error: astJson });
        return;
      }

      const ast = JSON.parse(astJson);
      // Evaluate AST in JS
      const result = await runAstProgram(ast);
      self.postMessage({ type: 'result', result: prettyPrint(result) });
    } catch (err) {
      self.postMessage({ type: 'error', error: String(err) });
    }

  } catch (err) {
    self.postMessage({ type: 'error', error: String(err) });
  }
});

function prettyPrint(v) {
  if (v === null || v === undefined) return 'null';
  if (Array.isArray(v)) return '[' + v.map(prettyPrint).join(', ') + ']';
  if (typeof v === 'object') {
    if (v._tag === 'Cons') {
      // treat as list cons
      const arr = [];
      let cur = v;
      while (cur && cur._tag === 'Cons') { arr.push(cur.head); cur = cur.tail; }
      return '[' + arr.map(prettyPrint).join(', ') + ']';
    }
    if (v.type === 'milangFunc' || v.type === 'nativeFunc') return '<function>'; 
    try { return JSON.stringify(v); } catch (e) { return String(v); }
  }
  return String(v);
}

function runAstProgram(ast) {
  if (!ast || ast.tag !== 'Namespace') throw new Error('Expected Namespace AST');
  const bindings = ast.bindings || [];
  const env = makeBaseEnv();
  // placeholders
  for (const b of bindings) {
    env[b.name] = undefined;
  }
  // create functions for paramful bindings first
  for (const b of bindings) {
    if (b.params && b.params.length > 0) {
      env[b.name] = makeMilangFunc(b.params, b.body, env);
    } else {
      // evaluate body now
      const val = evalExpr(b.body, env);
      env[b.name] = val;
    }
  }

  if (env['_main'] !== undefined) return env['_main'];
  if (env['main'] && env['main'].type === 'milangFunc') {
    // call main with a synthetic world object
    const world = makeWorld();
    const res = applyValue(env['main'], world);
    return res;
  }

  // otherwise return last non-internal binding value
  for (let i = bindings.length - 1; i >= 0; i--) {
    const name = bindings[i].name;
    if (!name.startsWith('_stmt_')) return env[name];
  }
  return null;
}

function makeWorld() {
  return {
    io: {
      println: (msg) => {
        self.postMessage({ type: 'log', msg: String(msg) });
      }
    }
  };
}

function makeBaseEnv() {
  const env = Object.create(null);
  // arithmetic as native functions (curried)
  env['+'] = makeNativeFunc(2, (args) => args[0] + args[1]);
  env['-'] = makeNativeFunc(2, (args) => args[0] - args[1]);
  env['*'] = makeNativeFunc(2, (args) => args[0] * args[1]);
  env['/'] = makeNativeFunc(2, (args) => args[0] / args[1]);
  env['=='] = makeNativeFunc(2, (args) => (args[0] === args[1]) ? 1 : 0);
  env['toString'] = makeNativeFunc(1, (args) => String(args[0]));
  // basic list constructors
  env['Nil'] = [];
  env['Cons'] = makeNativeFunc(2, (args) => ({_tag: 'Cons', head: args[0], tail: args[1]}));
  return env;
}

function makeNativeFunc(arity, impl, boundArgs = []) {
  return { type: 'nativeFunc', arity, impl, boundArgs };
}

function makeMilangFunc(params, body, env, boundArgs = []) {
  return { type: 'milangFunc', params, body, env, boundArgs };
}

function applyValue(fnVal, arg) {
  if (fnVal && fnVal.type === 'nativeFunc') {
    const newBound = (fnVal.boundArgs || []).concat([arg]);
    if (newBound.length < fnVal.arity) return makeNativeFunc(fnVal.arity, fnVal.impl, newBound);
    return fnVal.impl(newBound);
  } else if (fnVal && fnVal.type === 'milangFunc') {
    const newBound = (fnVal.boundArgs || []).concat([arg]);
    if (newBound.length < fnVal.params.length) return makeMilangFunc(fnVal.params, fnVal.body, fnVal.env, newBound);
    // fully applied: evaluate body with params bound
    const localEnv = Object.create(null);
    // param binding
    for (let i = 0; i < fnVal.params.length; i++) {
      localEnv[fnVal.params[i]] = newBound[i];
    }
    // parent env chain
    localEnv.__parent = fnVal.env;
    return evalExpr(fnVal.body, localEnv);
  } else if (typeof fnVal === 'function') {
    return fnVal(arg);
  } else {
    throw new Error('Not a function: ' + String(fnVal));
  }
}

function getVar(env, name) {
  if (env === null || env === undefined) return undefined;
  if (Object.prototype.hasOwnProperty.call(env, name)) return env[name];
  return getVar(env.__parent, name);
}

function evalExpr(node, env) {
  if (!node) return null;
  switch (node.tag) {
    case 'IntLit': return Number(node.value);
    case 'FloatLit': return Number(node.value);
    case 'StringLit': return node.value;
    case 'Name': {
      const v = getVar(env, node.name);
      if (v === undefined) throw new Error('Unbound name: ' + node.name);
      return v;
    }
    case 'BinOp': {
      const l = evalExpr(node.left, env);
      const r = evalExpr(node.right, env);
      switch (node.op) {
        case '+': return l + r;
        case '-': return l - r;
        case '*': return l * r;
        case '/': return l / r;
        case '==': return (l === r) ? 1 : 0;
        default: throw new Error('Unsupported operator: ' + node.op);
      }
    }
    case 'App': {
      const f = evalExpr(node.f, env);
      const x = evalExpr(node.x, env);
      return applyValue(f, x);
    }
    case 'Lam': {
      // create a milangFunc with single parameter
      return makeMilangFunc([node.param], node.body, env, []);
    }
    case 'Record': {
      const obj = {};
      for (const b of (node.bindings || [])) {
        obj[b.name] = evalExpr(b.body, env);
      }
      obj._tag = node.name || '';
      return obj;
    }
    case 'FieldAccess': {
      const o = evalExpr(node.expr, env);
      if (o === null || o === undefined) throw new Error('Field access on null');
      return o[node.field];
    }
    case 'ListLit': {
      return (node.items || []).map((it) => evalExpr(it, env));
    }
    case 'Namespace': {
      // evaluate into nested env
      const local = Object.create(null);
      local.__parent = env;
      for (const b of (node.bindings || [])) {
        if (b.params && b.params.length > 0) local[b.name] = makeMilangFunc(b.params, b.body, local, []);
      }
      for (const b of (node.bindings || [])) {
        if (!(b.params && b.params.length > 0)) local[b.name] = evalExpr(b.body, local);
      }
      return local;
    }
    default:
      throw new Error('Unhandled node tag: ' + node.tag);
  }
}
