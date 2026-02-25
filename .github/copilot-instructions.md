# Copilot Instructions for Milang

## What is Milang

Milang is a minimalist functional programming language with zero keywords. The compiler is written in Haskell, compiles Milang source (`.mi` files) to C via partial evaluation, then invokes `gcc`. Everything that looks like a keyword (`if`, `true`, `import`) is actually a function defined in the prelude.

## Build & Run

```bash
make          # build compiler, symlink ./milang in repo root
make test     # build then run all tests in parallel
make clean    # remove build artifacts
```

Run a single `.mi` file:

```bash
./milang run file.mi
```

Run a single test manually:

```bash
./milang run tests/arith.mi          # should exit 0
./milang run tests/type_errors.mi    # should fail; check stderr against tests/type_errors.mi.errors
```

Inspect compiler internals:

```bash
./milang dump file.mi      # parsed AST (before imports/reduction)
./milang reduce file.mi    # AST after partial evaluation
./milang compile file.mi   # emit standalone C to file.c
./milang repl              # interactive REPL
```

Docs are built with mdBook: `make docs` or `cd docs && mdbook serve --open`.

## Compiler Architecture

The compiler lives in `hs/` and is built with Cabal (`hs/milang.cabal`). The pipeline is:

```
Parser → Import Resolution → Prelude Injection → Type Check → Partial Evaluation → Type Check → Trait Check → C Codegen → gcc
```

Key modules in `hs/src/Milang/`:

| Module | Role |
|--------|------|
| `Syntax.hs` | Core AST: `Expr` type (IntLit, App, Lam, Record, Case, Namespace, etc.) and `Binding` record |
| `Lexer.hs` | Megaparsec-based tokenizer (indentation-sensitive) |
| `Parser.hs` | Parses `.mi` source into `Expr`; manages operator precedence via `:!` declarations |
| `Import.hs` | Resolves `import "path"` (local files, URLs, C headers); returns `LinkInfo` with gcc flags |
| `Remote.hs` | Fetches URLs, computes SHA-256 Merkle hashes for pinning |
| `CHeader.hs` | Parses C `.h` files for FFI function signatures |
| `Prelude.hs` | Hardcoded Milang prelude source (Bool, List, Maybe, `if`, arithmetic, etc.) |
| `Reduce.hs` | Partial evaluator — reduces compile-time-known expressions to values |
| `TypeCheck.hs` | Type inference/checking; runs both pre- and post-reduction |
| `TraitCheck.hs` | Validates capability/effect annotations (`:~` domain, world operations) |
| `Codegen.hs` | Emits self-contained C with embedded runtime (arena allocator, tagged unions, closures) |

`Main.hs` wires the pipeline and implements CLI commands (`run`, `compile`, `dump`, `reduce`, `pin`, `repl`).

## Test Conventions

Tests live in `tests/`. The Makefile runs all `tests/*.mi` in parallel.

**Two test patterns:**

1. **Passing tests** — a `.mi` file that must exit 0 when run with `./milang run`. Some have `.expected` files for stdout verification.
2. **Error tests** — a `.mi` file paired with a `.mi.errors` file. The program must fail, and every line in the `.errors` file must appear as a substring in stderr. Lines starting with `#` or blank lines are ignored.

`tests/lib/` contains importable helper modules used by test files.

## Milang Language Conventions

- **Zero keywords**: control flow (`if`/`else`), booleans, `import` — all are prelude functions.
- **Five annotation domains**: `=` (value), `::` (type), `:~` (traits), `:?` (docs), `:!` (parse declarations for custom operators).
- **Indentation-sensitive**: nested blocks are determined by whitespace.
- **Capability-based IO**: `main world = ...` receives a world record; pass `world.io`, `world.process`, etc. to restrict what functions can do.
- **Record literals as arguments** must be parenthesized: `f ({x = 1})`, not `f {x = 1}`.
- **Partial evaluation** is the optimizer — any expression fully known at compile time is reduced to a literal before codegen.
