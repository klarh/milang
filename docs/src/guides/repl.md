# REPL

The milang REPL (Read-Eval-Print Loop) lets you evaluate expressions and define bindings interactively.

## Starting the REPL

```bash
./milang repl
```

You'll see a `>` prompt. Type any expression and press Enter to evaluate it:

```text
> 2 + 3
5
> "hello" + " " + "world"
hello world
```

Press **Ctrl-D** to exit.

## Defining Bindings

Bindings persist across lines, so you can build up definitions:

```milang
> double x = x * 2
> double 21
42
> quadruple x = double (double x)
> quadruple 5
20
```

You can also define records, ADTs, and use pattern matching:

```milang
> Shape = {Circle radius; Rect width height}
> area s = s -> Circle = 3.14 * s.radius * s.radius; Rect = s.width * s.height
> area (Circle 5)
78.5
```

## Multi-line Input

The REPL automatically detects when a line is incomplete and waits for continuation. A line is considered incomplete when it:

- Has unclosed delimiters: `(`, `[`, or `{`
- Ends with an operator or `->`
- Ends with a comma
- Ends with a backslash `\`

```milang
> map (\x =
|   x * 2
| ) [1, 2, 3]
[2, 4, 6]
```

## Prelude Functions

All standard prelude functions are available immediately — no imports needed:

```milang
> map (\x = x * x) [1, 2, 3, 4, 5]
[1, 4, 9, 16, 25]
> filter (\x = x > 3) [1, 2, 3, 4, 5]
[4, 5]
> foldl (\acc x = acc + x) 0 [1, 2, 3]
6
> length [10, 20, 30]
3
```

## Type Annotations

You can add type annotations to bindings:

```milang
> x :: Int
> x = 42
```

The type is associated with the binding and checked when the value is defined.

## How It Works

Under the hood, each REPL input is:

1. Parsed as either a binding or an expression
2. Merged with all previous bindings
3. Compiled to a temporary C file
4. Compiled with `gcc` and executed
5. The output is printed

Bare expressions (not bindings) are internally bound to `_it` and their value is displayed.

## Command History

The REPL saves command history to `~/.local/share/milang/history`, so previous inputs are available with the up/down arrow keys across sessions.

## Limitations

- **No IO** — the REPL evaluates pure expressions only. There is no `world` value available, so `world.io.println` and similar IO operations cannot be used.
- **No imports** — `import` declarations are not supported in the REPL.
- **Compilation per input** — each line triggers a full compile-and-run cycle via gcc, so there is a small delay on each evaluation.

For IO and imports, write a `.mi` file and use `milang run` instead.
