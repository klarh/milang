self.addEventListener('message', (e) => {
  const msg = e.data;
  if (msg.type === 'run') {
    const code = msg.code || '';
    const steps = typeof msg.steps === 'number' ? msg.steps : 100000;
    try {
      const ast = parse(code);
      const result = evaluate(ast, standardEnv(), { steps });
      self.postMessage({ type: 'result', result: serialize(result) });
    } catch (err) {
      self.postMessage({ type: 'error', error: String(err) });
    }
  }
});

function tokenize(input) {
  const re = /\s*([()]|;.*|[^\s()]+)/g;
  const tokens = [];
  let m;
  while ((m = re.exec(input)) !== null) {
    const t = m[1];
    if (t[0] === ';') continue; // comment
    tokens.push(t);
  }
  return tokens;
}

function parse(input) {
  const tokens = tokenize(input);
  let i = 0;
  function parseExpr() {
    if (i >= tokens.length) throw new Error('Unexpected EOF');
    const tok = tokens[i++];
    if (tok === '(') {
      const list = [];
      while (tokens[i] !== ')' && i < tokens.length) {
        list.push(parseExpr());
      }
      if (tokens[i] !== ')') throw new Error('Expected )');
      i++; // consume )
      return { type: 'list', elements: list };
    } else if (tok === ')') {
      throw new Error('Unexpected )');
    } else {
      const num = Number(tok);
      if (!Number.isNaN(num)) return { type: 'number', value: num };
      return { type: 'symbol', name: tok };
    }
  }
  const exprs = [];
  while (i < tokens.length) exprs.push(parseExpr());
  return exprs.length === 1 ? exprs[0] : { type: 'list', elements: [ { type:'symbol', name: 'begin' }, ...exprs ] };
}

function standardEnv() {
  const env = Object.create(null);
  env['+'] = (a,b) => a + b;
  env['-'] = (a,b) => a - b;
  env['*'] = (a,b) => a * b;
  env['/'] = (a,b) => a / b;
  env['print'] = (...args) => { self.postMessage({ type: 'log', msg: args.join(' ') }); return null; };
  env['>'] = (a,b) => a > b;
  env['<'] = (a,b) => a < b;
  env['='] = (a,b) => a === b;
  return env;
}

function serialize(x) {
  if (x === null || x === undefined) return 'null';
  if (typeof x === 'object') return JSON.stringify(x);
  return String(x);
}

function evaluate(node, env, opts) {
  const steps = { val: opts.steps || 100000 };
  function _eval(n, e) {
    if (steps.val-- <= 0) throw new Error('step limit exceeded');
    if (n.type === 'number') return n.value;
    if (n.type === 'symbol') {
      if (n.name in e) return e[n.name];
      // look up in chain:
      let cur = e.__parent;
      while (cur) {
        if (n.name in cur) return cur[n.name];
        cur = cur.__parent;
      }
      throw new Error('Unbound symbol: ' + n.name);
    }
    if (n.type === 'list') {
      if (n.elements.length === 0) return null;
      const first = n.elements[0];
      if (first.type === 'symbol') {
        const name = first.name;
        if (name === 'if') {
          const cond = _eval(n.elements[1], e);
          return cond ? _eval(n.elements[2], e) : _eval(n.elements[3], e);
        }
        if (name === 'lambda') {
          const params = n.elements[1].elements.map(p => p.name);
          const body = n.elements[2];
          return { type: 'closure', params, body, env: e };
        }
        if (name === 'define') {
          const sym = n.elements[1].name;
          const val = _eval(n.elements[2], e);
          e[sym] = val;
          return val;
        }
        if (name === 'begin') {
          let res = null;
          for (let i = 1; i < n.elements.length; i++) res = _eval(n.elements[i], e);
          return res;
        }
      }
      // function application
      const fn = _eval(first, e);
      if (typeof fn === 'function') {
        const args = [];
        for (let i = 1; i < n.elements.length; i++) args.push(_eval(n.elements[i], e));
        return fn(...args);
      } else if (fn && fn.type === 'closure') {
        const newEnv = Object.create(null);
        for (let i = 0; i < fn.params.length; i++) {
          newEnv[fn.params[i]] = _eval(n.elements[i+1], e);
        }
        newEnv.__parent = fn.env;
        return _eval(fn.body, newEnv);
      } else {
        throw new Error('Not a function');
      }
    }
    throw new Error('Unknown node type: ' + n.type);
  }
  return _eval(node, env);
}
