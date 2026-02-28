# Compiler Modes

The `milang` binary supports several commands. Run `milang --help` for an overview or `milang <command> --help` for per-command options.

## `milang run`

```bash
milang run file.mi
milang run file.mi arg1 arg2      # pass arguments to the compiled program
milang run --keep-c file.mi       # keep the generated _core.c file after execution
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
milang compile file.mi              # writes file.c
milang compile file.mi -o output.c  # writes output.c
milang compile file.mi -o -         # prints C to stdout
```

Emits a self-contained C source file. The milang runtime is embedded in the output, so the generated file has no external dependencies beyond the standard C library.

Compile it yourself:

```bash
gcc output.c -o program
./program
```

This is useful when you want to inspect the generated code, cross-compile, or integrate milang output into a larger C project.

## `milang reduce`

```bash
milang reduce file.mi               # fully reduced AST (with prelude)
milang reduce --no-reduce file.mi   # parsed AST only (no reduction)
milang reduce --no-prelude file.mi  # reduced AST without prelude injection
milang reduce --json file.mi        # output structured JSON IR
milang reduce --json -o ir.json file.mi  # write JSON IR to a file
```

The `reduce` command is the primary AST inspection tool. By default it shows the AST **after partial evaluation** with the prelude injected — this is exactly what the code generator sees. Any expression that was fully known at compile time has been reduced to a literal.

Flags control how much of the pipeline runs:

| Flag | Effect |
|------|--------|
| *(none)* | Parse → import resolution → prelude → reduce |
| `--no-reduce` | Parse only; no imports or reduction (formerly `dump`) |
| `--no-prelude` | Parse → imports → reduce, without prelude (formerly `raw-reduce`) |
| `--json` | Emit a structured JSON IR instead of pretty-printed text |
| `-o FILE` | Write output to `FILE` instead of stdout |

Flags compose freely: `--no-reduce --json` gives you the raw parsed AST as JSON, and `--no-prelude --json` gives you the reduced AST of just your file without the standard library.

### Example

```milang,run
square x = x * 2
answer = square 21
```

Running `milang reduce` on this file shows `answer = 42` — the function call was evaluated at compile time.

Running `milang reduce --no-reduce` shows the unevaluated parse tree with `answer = (square 21)`.

### JSON IR

`milang reduce --json` serialises the reduced `Expr` tree to a stable JSON format. This is the foundation for building alternative backends — Python interpreters, C++ code generators, WASM targets, etc. — without needing to embed the Haskell compiler.

Every node has a `"tag"` field for dispatch. Example output for `x = 13 * 7`:

```json
{
  "tag": "namespace",
  "bindings": [{
    "domain": "value",
    "name": "x",
    "params": [],
    "body": {
      "tag": "binop",
      "op": "*",
      "left":  { "tag": "int", "value": 13 },
      "right": { "tag": "int", "value": 7 }
    }
  }]
}
```

After full reduction, `--json` produces `{ "tag": "int", "value": 91 }` for the `x` binding body.

### Backward-compatible aliases

`milang dump` and `milang raw-reduce` are kept as aliases:

```bash
milang dump file.mi          # same as: milang reduce --no-reduce
milang raw-reduce file.mi    # same as: milang reduce --no-prelude
```

## `milang pin`

```bash
milang pin file.mi
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

This ensures that the imported code hasn't changed since you last pinned it. If the content changes, the compiler will report a hash mismatch and refuse to proceed.

## `milang repl`

```bash
milang repl
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
| `compile file.mi [-o out.c]` | Emit standalone C |
| `reduce file.mi [flags]` | Inspect AST (pretty-print or JSON) |
| `pin file.mi` | Fetch URL imports, write SHA-256 hashes |
| `repl` | Interactive evaluation |
