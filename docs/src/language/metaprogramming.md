# Metaprogramming

Milang provides two complementary operators for working with syntax at compile
time: **quote** (`#`) captures an expression as a data structure, and **splice**
(`$`) evaluates a data structure back into code. Combined with partial
evaluation these give you compile-time code generation without a separate macro
system.

## Quote: `#expr`

The `#` operator captures the *abstract syntax tree* of its operand as a tagged
record. The expression is **not** evaluated — only its structure is recorded.

```milang,run
q_int = #42
q_op  = #(1 + 2)
```

Each syntactic form maps to a specific record `tag`:

| Syntax | Quoted form |
|---|---|
| `#42` | `Int {val = 42}` |
| `#"hello"` | `Str {val = "hello"}` |
| `#x` | `Var {name = "x"}` |
| `#(f x)` | `App {fn = Var {name = "f"}; arg = Var {name = "x"}}` |
| `#(a + b)` | `Op {op = "+"; left = ...; right = ...}` |
| `#(\x -> x)` | `Fn {param = "x"; body = ...}` |

Because quoted ASTs are ordinary records you can inspect their `fields`, pass them
to functions, and build new ASTs by constructing records directly.

## Splice: `$expr`

The `$` operator takes a record that represents an AST node and evaluates it as
code:

```milang,run
ast = #(1 + 2)
result = $ast
```

Splicing a quoted literal round-trips back to its value. More usefully, you can
build AST records by hand and splice them:

```milang,run
ast = Op {op = "*"; left = Int {val = 6}; right = Int {val = 7}}
answer = $ast
```

## Writing macros

A macro in milang is just a function that takes and returns AST records. Because
the partial evaluator runs at compile time, macro expansion happens before code
generation — there is no runtime cost.

```milang,run
-- Macro: double an expression (x + x)
double_ast expr = Op {op = "+"; left = expr; right = expr}
r1 = $(double_ast #5)

-- Macro: negate (0 - x)
negate_ast expr = Op {op = "-"; left = Int {val = 0}; right = expr}
r2 = $(negate_ast #42)
```

Macros compose — you can pass one macro's output as another's input:

```milang,run
double_ast expr = Op {op = "+"; left = expr; right = expr}
negate_ast expr = Op {op = "-"; left = Int {val = 0}; right = expr}
r = $(double_ast (negate_ast #7))
```

## Pattern matching on ASTs

Because quoted expressions are records, you can pattern-match on them to
transform code structurally:

```milang
-- Swap the arguments of a binary operator
swap_op ast = ast ->
  Op {op = op; left = l; right = r} = Op {op = op; left = r; right = l}
  _ = ast
```

## Auto-Quote Parameters (`#param`)

When defining a function (or binding), prefixing a parameter name with `#`
tells the compiler to **automatically quote** the corresponding argument at the
call site. Inside the body, `$param` splices the captured AST back into code
and evaluates it. The caller writes ordinary expressions — no sigils needed.

```milang,run
-- 'if' is defined in the prelude using auto-quote params:
--   if cond #t #e = (truthy cond) -> 0 = $e; _ = $t
-- The caller just writes:
x = 10
result = if (x > 5) (x * 2) (x * 3)
```

Because the `#t` and `#e` parameters auto-quote their arguments, neither
branch is evaluated until the matching `$t` or `$e` splice runs. This is how
milang achieves lazy branching without keywords or special forms — `if` is an
ordinary user-defined function.

Auto-quote parameters work with any function, not just `if`:

```milang
-- A logging wrapper that only evaluates its message when enabled
log_if enabled #msg world = if enabled (world.io.println $msg) 0
```

Note that `world` must be threaded through explicitly: `world.io.println` requires the `world` value to be in scope, so any function calling IO must accept it as a parameter.

### How it works

1. The function definition declares `#param` — the `#` is part of the parameter
   name in the source but is stripped for binding purposes.
2. At each call site, the compiler wraps the corresponding argument in `#(...)`,
   producing a quoted AST record.
3. In the body, `$param` splices the record back into an expression and
   evaluates it in the current environment.
4. If the spliced expression contains a thunk (`~expr`), the thunk is
   automatically forced after splicing — so old-style `~` code remains
   backward-compatible.

### Relation to thunks

Auto-quote parameters are strictly more general than thunks (`~`). A thunk
delays evaluation of a single expression; a quoted parameter captures the full
AST, which can be inspected, transformed, or conditionally evaluated. For
simple conditional laziness (like `if`), the effect is the same — but
auto-quote opens the door to user-defined control flow, macros that inspect
their arguments, and other metaprogramming patterns.

## Inspection commands

Two CLI commands help you understand what the compiler sees:

```bash
milang dump file.mi      -- show the parsed AST (before reduction)
milang reduce file.mi    -- show the AST after partial evaluation
```

Use `dump` to verify that quoting produces the record structure you expect, and
`reduce` to confirm that your macros expand correctly at compile time.
