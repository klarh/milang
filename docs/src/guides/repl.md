# REPL

The milang REPL (Read-Eval-Print Loop) lets you evaluate expressions and define bindings interactively.

## Starting the REPL

```bash
./milang repl
```

You'll see a `λ>` prompt. Type any expression and press Enter to evaluate it:

```text
λ> 2 + 3
5
λ> "hello" + " " + "world"
"hello world"
```

Type `:q` or press **Ctrl-D** to exit.

## Defining Bindings

Bindings persist across inputs, so you can build up definitions incrementally:

```text
λ> double x = x * 2
double = (\x -> (x * 2))
λ> double 21
42
λ> quadruple x = double (double x)
quadruple = (\x -> (double (double x)))
λ> quadruple 5
20
```

## Single-line Input

The REPL reads one line at a time. Each binding must fit on a single line. Use semicolons to separate alternatives within a `->` pattern match:

```text
λ> area s = s -> Circle = 3.14 * s.radius * s.radius; Rect = s.width * s.height
```

Multi-line indented definitions must be written in a `.mi` file and loaded via `milang run`.

## Prelude Functions

All standard prelude functions are available immediately — no imports needed:

```text
λ> map (\x = x * x) [1, 2, 3, 4, 5]
Cons {head = 1, tail = Cons {head = 4, tail = ...}}
λ> filter (\x = x > 3) [1, 2, 3, 4, 5]
Cons {head = 4, tail = Cons {head = 5, tail = Nil {}}}
λ> fold (\acc x = acc + x) 0 [1, 2, 3]
6
λ> len [10, 20, 30]
3
```

> **Note:** Lists are displayed as raw `Cons`/`Nil` record expressions — the REPL shows the partially-evaluated AST, not a pretty-printed representation.

## Type Annotations

You can add type annotations to bindings:

```text
λ> x :: Int
λ> x = 42
x = 42
```

The type is associated with the binding and checked when the value is defined.

## Viewing Bindings

Use `:env` to show all user-defined bindings (prelude bindings are hidden):

```text
λ> double x = x * 2
double = (\x -> (x * 2))
λ> :env
double = (\x -> (x * 2))
```

## How It Works

Each REPL input is:

1. Parsed as either a binding (namespace) or a bare expression
2. Reduced using the same partial evaluator as `milang reduce`
3. The reduced form is printed

New bindings extend the accumulated environment for all subsequent inputs. This is a **pure partial evaluator** — there is no C compilation or gcc invocation in the REPL. Residuals (expressions that cannot be further reduced) are printed as-is.

## Limitations

- **No IO** — the REPL evaluates pure expressions only. There is no `world` value available, so `world.io.println` and similar IO operations cannot be used.
- **No imports** — `import` declarations are not supported in the REPL.
- **No multi-line input** — each input must fit on a single line. Write multi-line programs in `.mi` files.
- **No command history** — the up/down arrow keys do not recall previous inputs.
- **Raw list output** — lists are printed as `Cons`/`Nil` record expressions, not `[1, 2, 3]`.

For IO and imports, write a `.mi` file and use `milang run` instead.

## Browser Sandbox

A browser-based sandbox is available at [/repl.html](/repl.html). It uses a
WebAssembly build of the milang compiler to reduce programs in the browser,
with no server round-trips. Multi-line programs with helper function definitions
and a trailing expression are supported.

```text
-- Define a helper and apply it
double x = 2 * x

map double [1, 2, 3, 4, 5]
```

The sandbox shows the reduced (partially-evaluated) form of the last expression.
