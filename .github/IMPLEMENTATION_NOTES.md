# Implementation Notes

Non-obvious details about the milang compiler internals, collected during development. Read this before making changes to the compiler.

## Build Requirements

- **GHC 9.6.6** is the target compiler version.
- **CRITICAL**: The `core/milang-core.cabal` file must include `-dynamic` in `ghc-options`. Without it, GHC 9.6.6 on some Linux distros produces cryptic "files missing in base" link errors. If the build fails with linker errors about missing `.so` files, check this first.
- Build: `cd core && cabal build`
- Run: `$(cd core && cabal list-bin milang-core) run <file>`
- The top-level `Makefile` symlinks `./milang` to the core binary.

## Core Architecture: Unified Reduction

The core compiler's central insight is that **all compilation phases** share one partial evaluator (`Core.Reduce.reduceD`). The `Binding` type carries a `Domain` tag that tells the reducer how to handle it:

```haskell
data Domain = Value | Lazy | Type | Trait | Doc | Parse
```

Type checking and trait checking happen *during reduction*, not as separate passes. After reducing all bindings in a namespace (using SCC ordering for dependency analysis), the reducer runs `typeCheckBindings` and `traitCheckBindings` against the accumulated environment.

**Pure by default**: Functions without a `:~` trait annotation are assumed pure (`:~ []`). Any function that performs IO must explicitly declare its effects. `main` and `_main` (the auto-wrapper for main-less files) are the only exceptions — they are implicitly granted all capabilities.

### Why This Matters

- Adding a new annotation domain (e.g., for effects, ownership) only requires adding a `Domain` variant and a few clauses in `evalAnnotation` and the checker functions.
- The reducer is ~1500 lines and is the most actively modified file. Changes here affect everything.
- The `Env` type accumulates both bindings and warnings — warnings are extracted at the top level in `reduce`/`reduceWithEnv`.

## Quoted Parameters (#param)

Quoted parameters solve short-circuit evaluation without special-casing `if`/`&&`/`||`:

```milang
if cond #t #e = ...    -- #t and #e are auto-quoted (not evaluated at call site)
```

### How it works:
1. **Parser**: `#name` is stored as a parameter name with `#` prefix
2. **`isQuotedParam`**: checks `T.isPrefixOf "#"`
3. **`reduceApp (Lam p body) arg | isQuotedParam p`**: calls `quoteExpr arg` to reify the argument as AST data, then binds the quoted form to `lamParamName p` (strips `#`)
4. **`Lam` reduction**: when a lambda has a quoted param, alpha-renames the real param to a fresh name to prevent env capture
5. **`quoteExpr`/`spliceExprM`**: convert between AST and data records (round-trip)

### Critical gotcha: Splice must stay residual for unresolved Names
When reducing `Splice body`: if `body` reduces to `Name _`, keep as `Splice (Name _)` (residual). Without this, lambda bodies containing `$t` get prematurely evaluated when `t` isn't bound yet.

### Critical gotcha: Quote/splice round-trip for compound types
`quoteExpr (ListLit es)` produces `Record "List" [items = listToCons(map quoteExpr es)]`. The corresponding `spliceExprM (Record "List" ...)` **must** recursively unquote items via `consToList` + `map spliceExpr`. Same for `Record "Rec"` and `Record "Case"`. Without this, quoted items leak as AST records.

## Type Checking Integration

Type checking is done by comparing `::` annotations against reduced values. Key details:

- **`expandUnion` constructor leak**: When type-checking, use the *original* bindings, not the expanded ones. If you use `expanded` (which includes generated constructors from union declarations), the constructors override `detectUnion`'s arity-0 type detection.
- **Prelude annotation leak**: `collectTypeEnv` must distinguish prelude vs user annotations via `isPreludePos`. Without this, prelude type annotations (which use placeholder types) contaminate user type checking.
- **False positive root cause**: If type errors appear for correct code, check whether `expandUnion` output is being used for type checking instead of the original `bindings`.

## Sized Types

`SizedInt !Integer !Integer !Bool` (value, bit-width, isSigned) and `SizedFloat !Double !Integer` (value, bit-width).

- Only propagated through arithmetic (`sizedIntResult`/`sizedFloatResult`), never created from type annotations
- Prelude's `Int = Int' 64`, `UInt = UInt' 64`, etc. are **type-domain aliases** used by the type checker only
- `intVal`/`floatVal` extractors handle both plain and sized types transparently
- Overflow clamping: `clampSigned w v = ((v + 2^(w-1)) mod 2^w) - 2^(w-1)`

## Import Resolution

Import resolution happens in `Main.hs` via `resolveImports`/`resolveExpr`, which walks the AST and resolves `Import` nodes before reduction.

- **`ResCtx`**: carries cache, in-progress set (for circular import detection), link info, Merkle hash map, and hash errors
- **URL imports**: detected by `isURL` (checks `https://` or `http://` prefix), cached in `~/.cache/milang/<sha256-of-url>/<basename>`
- **Merkle hashes**: `SHA256(content_hash ++ sorted_sub_import_merkle_hashes)` — content-addressed with transitive dependency tracking
- **Circular imports**: detected via the in-progress set; resolved using `Name` references that get filled in later

## C Codegen

The codegen emits self-contained C with an embedded runtime. Key facts:

- The `hidden` set (prelude names + expanded constructors) controls which bindings get printed in script mode
- `main world = ...` pattern: if `main` takes an argument, codegen builds a world record and calls `mi_apply(main, world)`; otherwise it prints all non-hidden bindings
- Lazy bindings (`:=`) are wrapped in thunks (closures with `"_thunk_"` param)
- The C runtime uses arena allocation, tagged unions (`MiVal`), and a simple env chain

## Test Infrastructure

The test comparison infrastructure runs both compilers on each `.mi` file and diffs output. Key details:

- Error tests (`.mi.errors`) check for **substring matches** in stderr — don't rely on exact error message formatting
- The error message format is: `error: file.mi:line:col: name: message`
- Prelude warnings are filtered out before display (checked via `isPreludeWarning`/`isPosPrelude`)
- Warnings are deduplicated by source line (`dedupByLine`)

## Common Pitfalls

1. **Adding new `Expr` constructors**: Must update `prettyExpr`, `isResidual`, `isConcrete`, `exprFreeVars`, `substExpr`, `quoteExpr`, `spliceExprM`, `matchPat` (if applicable), and `exprToC` in Codegen.
2. **Modifying `reduceD`**: This function is pure (`Int -> Env -> Expr -> Expr`). It cannot accumulate warnings. Warnings are only emitted in `evalSCC` (which has access to the mutable `Env`).
3. **Operator precedence**: Custom operators use `:!` parse declarations. The parser pre-scans for these before parsing. If tests break after adding operators, check `scanParseDecls`.
4. **Thunk forcing**: `forceThunk d env e` evaluates thunks (closures with `"_thunk_"` param). BinOp operands are force-thunked before reduction. Missing `forceThunk` calls cause thunks to appear as closures in output.
5. **Call-by-name vs call-by-value**: Regular params are call-by-value (reduced before binding). Quoted params (`#name`) are call-by-name (argument is quoted, not reduced). The `App` reduction for non-quoted non-lambda falls through to residual — this is intentional for recursive functions.
