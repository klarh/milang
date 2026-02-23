# Hello World

This guide walks through creating, running, and compiling your first Milang program and explains common variants useful when learning the language.

## Your First Program

Create a file called hello.mi with this content:

```milang,run
main world =
  world.io.println "Hello, Milang!"
```

Run it with the bundled binary:

```bash
./milang run hello.mi
```

Expected output:

```
Hello, Milang!
```

### What `main` and `world` mean

- `main` is the program entry point by convention (not a language keyword).
- `world` is an explicit record that carries runtime capabilities: `world.io` (console and file IO), `world.process` (exec/exit), `world.argv`, and helpers like `getEnv`.
- Only code that receives the appropriate part of `world` can perform the corresponding effects — pass only what you need to follow the principle of least privilege.

## Printing and Helpers

`println` appends a newline; `print` does not. Prefer small helpers that accept only the sub-record they need:

```milang,run
greet io name = io.println ("Hello, " + name + "!")

main world =
  greet world.io "Alice"
```

This makes `greet` unable to access process or filesystem capabilities.

## Script Mode (quick experiments)

When a file does not define `main` that takes a parameter, `milang run` executes in script mode: every top-level binding is evaluated and printed. This is ideal for short tests and REPL-style exploration.

```milang,run
x = 6 * 7
y = x + 1
```

Script-mode output prints name/value pairs for top-level bindings (prelude/internal bindings are hidden).

## Printing non-strings and Maybe values

Use `toString` to render non-string values. Many standard library functions return `Maybe` — e.g., `toInt`, `toFloat`, `charAt`, and `getField` — so pattern-match on `Just`/`Nothing` when appropriate.

```milang,run
main world =
  world.io.println (toString (toInt "42"))
  world.io.println (toString (toInt "abc"))
```

## Compiling to C

Emit the generated C and compile it:

```bash
./milang compile hello.mi hello.c
gcc hello.c -o hello
./hello
```

The C file embeds the milang runtime; you only need a standard C toolchain.

## Using the REPL

Start the REPL for interactive experimentation:

```bash
./milang repl
```

Example session:

```text
> 2 + 3
5
> f x = x * x
> f 8
64
> map f [1, 2, 3, 4]
[1, 4, 9, 16]
```

Bindings persist across lines; rethink and refine definitions live.

## Next Steps

- Read the full [syntax cheatsheet](../language/cheatsheet.md).
- Inspect reduction with `./milang reduce` (see [Partial Evaluation](../language/partial-eval.md)).
- Try the larger examples in the repository root.
