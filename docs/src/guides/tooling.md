# Tooling & Build Notes

This page collects practical tips for building Milang programs and the documentation.

## Running examples and the CLI

- Use the local `./milang` binary in the repository root for running and experimenting with `.mi` files; if you installed `milang` on your PATH you can omit `./`.
- Useful commands:
  - `./milang run file.mi` — compile and run
  - `./milang compile file.mi output.c` — emit standalone C
  - `./milang dump file.mi` — show parsed AST (before reduction)
  - `./milang reduce file.mi` — show partially-evaluated AST (what the codegen sees)
  - `./milang repl` — interactive REPL

## C toolchain

Milang emits C and requires a working C toolchain (gcc or clang). On Debian/Ubuntu:

```bash
sudo apt-get install build-essential pkg-config
```



## Building the docs (mdBook)

The repo includes an `mdbook`-style source under `docs/src` and a small preprocessor `docs/mdbook-milang.py` that executes code blocks tagged `milang,run` and appends their output. Two ways to build:

- If the project has a `Makefile`, run `make docs` (required if available).
- Or, install mdBook and run:

```bash
mdbook build docs/src -d docs/out
```

Ensure your mdBook configuration registers the `mdbook-milang.py` preprocessor, or run the script manually when verifying examples.

## Common issues & debugging

- Parsing ambiguity with inline record literals: when passing a record literal directly as an argument, parenthesize it or bind it to a name, e.g. `getField ({a = 1}) "a"` or `r = {a = 1}
getField r "a"`.
- `toInt` / `toFloat` return `Nothing` on parse failure — check results with pattern matching on `Just` / `Nothing`.
- `charAt` and `getField` return `Nothing` when out-of-bounds or missing.
- Division/modulo by zero is handled at the runtime/C level (implementation-defined); avoid relying on undefined behaviour in portable code.
- Use `milang dump` to inspect how the parser grouped expressions if you hit unexpected parse errors.


## Quick checklist

- `./milang dump` to verify parser grouping
- `./milang reduce` to verify partial evaluation
- `mdbook build` (or `make docs`) to render the site
