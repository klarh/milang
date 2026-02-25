# Copilot Instructions for Milang

## What is Milang

Milang is a minimalist functional programming language with zero keywords. The compiler is written in Haskell, compiles Milang source (`.mi` files) to C via partial evaluation, then invokes `gcc`. Everything that looks like a keyword (`if`, `true`, `import`) is actually a function defined in the prelude.

## Implementations

There are two Haskell implementations:

- **`core/`** — The main implementation. Uses a unified "reduce-and-leave-residuals" architecture where parsing, type checking, trait checking, and code generation all share one partial evaluator. This is what `make` builds and `./milang` points to.
- **`hs/`** — The original reference implementation with separate passes for each compilation phase. Kept for comparison; build with `make test-hs`.

Both produce identical output on all 67 tests.

## Build & Run

```bash
make          # build core compiler, symlink ./milang in repo root
make test     # build then run all tests in parallel
make test-hs  # build and test the hs reference implementation
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
./milang pin file.mi       # add sha256 hashes to URL imports
./milang --help            # show all commands
```

Docs are built with mdBook: `make docs` or `cd docs && mdbook serve --open`.

## Core Compiler Architecture

The core compiler lives in `core/` and is built with Cabal (`core/milang-core.cabal`). The pipeline is:

```
Parser → Import Resolution → Prelude Injection → Unified Reduction (types + traits + values) → C Codegen → gcc
```

The key insight is that **all compilation phases** use the same partial evaluator. Type checking, trait checking, and value reduction are domain-tagged operations within one reducer, not separate passes.

Key modules in `core/src/Core/`:

| Module | Role |
|--------|------|
| `Syntax.hs` | Core AST: `Expr` type with `SizedInt`/`SizedFloat`, `Binding` with domain tags (`Value`/`Lazy`/`Type`/`Trait`/`Doc`/`Parse`) |
| `Lexer.hs` | Megaparsec-based tokenizer (indentation-sensitive) |
| `Parser.hs` | Parses `.mi` source into `Expr`; manages operator precedence via `:!` declarations |
| `Reduce.hs` | **The unified partial evaluator** — handles all domains, type/trait checking, quote/splice, pattern matching |
| `Prelude.hs` | Hardcoded Milang prelude source (Bool, List, Maybe, `if`, arithmetic, etc.) |
| `Codegen.hs` | Emits self-contained C with embedded runtime (arena allocator, tagged unions, closures) |
| `Remote.hs` | Fetches URLs, computes SHA-256 Merkle hashes for pinning |
| `CHeader.hs` | Parses C `.h` files for FFI function signatures |

`Main.hs` wires the pipeline and implements CLI commands, import resolution, and the REPL.

## hs Reference Implementation

The hs compiler lives in `hs/` with separate modules for each pass:

| Module | Role |
|--------|------|
| `Syntax.hs` | AST with optional type/trait/doc annotations per binding |
| `Reduce.hs` | Partial evaluator (values only) |
| `TypeCheck.hs` | Pre/post-reduction type inference and checking |
| `TraitCheck.hs` | Capability/effect validation |
| `Codegen.hs` | C code generation |

## Test Conventions

Tests live in `tests/`. The Makefile runs all `tests/*.mi` in parallel.

**Two test patterns:**

1. **Passing tests** — a `.mi` file that must exit 0 when run with `./milang run`.
2. **Error tests** — a `.mi` file paired with a `.mi.errors` file. The program must fail, and every line in the `.errors` file must appear as a substring in stderr. Lines starting with `#` or blank lines are ignored.

`tests/lib/` contains importable helper modules used by test files.

## Milang Language Conventions

- **Zero keywords**: control flow (`if`/`else`), booleans, `import` — all are prelude functions.
- **Six annotation domains**: `=` (value), `:=` (lazy), `::` (type), `:~` (traits), `:?` (docs), `:!` (parse declarations for custom operators).
- **Indentation-sensitive**: nested blocks are determined by whitespace.
- **Quoted parameters** (`#param`): auto-quote arguments for short-circuit evaluation (used by `if`, `&&`, `||`).
- **Capability-based IO**: `main world = ...` receives a world record; pass `world.io`, `world.process`, etc. to restrict what functions can do.
- **Record literals as arguments** must be parenthesized: `f ({x = 1})`, not `f {x = 1}`.
- **Partial evaluation** is the optimizer — any expression fully known at compile time is reduced to a literal before codegen.
