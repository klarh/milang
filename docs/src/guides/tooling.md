# Tooling & Build Notes

This page collects practical tips for building Milang programs and the documentation.

## Running examples and the CLI

- Use the local `./milang` binary in the repository root for running and experimenting with `.mi` files; if you installed `milang` on your PATH you can omit `./`.
- Run `milang --help` or `milang <command> --help` for full option listings.
- Useful commands:
  - `milang run file.mi` — compile and run
  - `milang run --keep-c file.mi` — compile and run, keeping the generated C file
  - `milang compile file.mi -o output.c` — emit standalone C
  - `milang reduce file.mi` — show partially-evaluated AST (what the codegen sees)
  - `milang reduce --no-reduce file.mi` — show parsed AST before reduction (formerly `dump`)
  - `milang reduce --no-prelude file.mi` — reduce without prelude injection (formerly `raw-reduce`)
  - `milang reduce --json file.mi` — output structured JSON IR for use in external backends
  - `milang repl` — interactive REPL

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
- Use `milang reduce --no-reduce` to inspect how the parser grouped expressions if you hit unexpected parse errors.


## Quick checklist

- `milang reduce --no-reduce` to verify parser grouping
- `milang reduce` to verify partial evaluation
- `milang reduce --json` to inspect the IR for backend development
- `mdbook build` (or `make docs`) to render the site
