# Compiler Modes

The `milang` binary supports six commands. Each operates on a `.mi` source file and exercises different parts of the compilation pipeline.

## `milang run`

```bash
./milang run file.mi
```

The most common command. It parses the file, resolves imports, partially evaluates, generates C, compiles with `gcc -O2`, runs the resulting binary, and cleans up temporary files.

If the file defines `main world = ...`, the program runs as a normal executable and `main`'s return value becomes the process exit code. If there is no `main` binding with a parameter, milang runs in **script mode** (see below).

### Example

```milang,run
main world =
  world.io.println "running!"
```

## `milang compile`

```bash
./milang compile file.mi            # writes file.c
./milang compile file.mi output.c   # writes output.c
./milang compile file.mi -          # prints C to stdout
```

Emits a self-contained C source file. The milang runtime is embedded in the output, so the generated file has no external dependencies beyond the standard C library.

Compile it yourself:

```bash
gcc output.c -o program
./program
```

This is useful when you want to inspect the generated code, cross-compile, or integrate milang output into a larger C project.

## `milang dump`

```bash
./milang dump file.mi
```

Prints the **parsed AST** before import resolution or partial evaluation. This shows you exactly how the parser interpreted your source, including all five annotation domains (`=`, `::`, `:~`, `:?`, `:!`).

Use this to debug syntax issues — if the parser grouped your expressions differently than you expected, `dump` will show it.

## `milang reduce`

```bash
./milang reduce file.mi
```

Prints the AST **after partial evaluation**. This is what the code generator actually sees. Any expression that could be computed at compile time has been reduced to a literal value.

Use this to verify that the partial evaluator is doing what you expect:

```milang,run
square x = x * 2
answer = square 21
```

Running `milang reduce` on this file would show `answer = 42` — the function call was evaluated at compile time.

If you need to see reduction without the prelude injected, use `milang raw-reduce file.mi` to view the reduced AST for the file alone.

## `milang pin`

```bash
./milang pin file.mi
```

Finds all URL imports in the file, fetches them, computes a Merkle hash (SHA-256 of the content plus all transitive sub-imports), and rewrites the source file to include the hash:

Before:
```milang
utils = import "https://example.com/utils.mi"
```

After:
```milang
utils = import' "https://example.com/utils.mi" ({sha256 = "a1b2c3..."})
```

This ensures that the imported code hasn't changed since you `last` pinned it. If the content changes, the compiler will report a hash mismatch and refuse to proceed.

## `milang repl`

```bash
./milang repl
```

Starts an interactive REPL where you can evaluate expressions and define bindings. See the [REPL](./repl.md) chapter for details.

## Script Mode

When a `.mi` file has no `main` binding that takes a parameter, `milang run` operates in **script mode**: it evaluates every top-level binding and prints each name-value pair.

```milang,run
a = 2 + 3
b = a * a
greeting = "hello"
```

Script mode is ideal for quick calculations and testing small snippets. Prelude definitions are automatically hidden from the output.

## Security Flags

Milang supports flags to restrict what imported code can do (note: these flags are not currently implemented in the core compiler; see .github/ROADMAP.md):

| Flag | Effect |
|------|--------|
| `--no-ffi` | Disables all C FFI imports (no `.h` files can be imported) |
| `--no-remote-ffi` | Disallows C FFI for remote (URL) imports specifically |

These flags are useful when running untrusted code. A URL import might try to `import "/usr/include/stdlib.h"` and call arbitrary C functions — `--no-remote-ffi` prevents this while still allowing your own local FFI usage.

## Summary

| Command | What it does |
|---------|-------------|
| `run file.mi` | Compile and execute |
| `compile file.mi [out.c]` | Emit standalone C |
| `dump file.mi` | Show parsed AST |
| `reduce file.mi` | Show partially-evaluated AST |
| `pin file.mi` | Fetch URL imports, write SHA-256 hashes |
| `repl` | Interactive evaluation |
