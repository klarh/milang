# Milang Roadmap

## Type System

Bidirectional `::` annotations, inference through `Case`/`With`/`App`, operator type compatibility, sized numeric types (`Int'`/`UInt'`/`Float'`), prelude type annotations, and trait validation (`:~`) are all implemented.

### Dependent & Refinement Types
- The partial evaluator already evaluates predicates at compile time, so refinement types can be expressed with minimal new machinery
- Planned syntax: `Positive = Int' 64 | (\x -> x > 0)`, `SmallInt = Int' 32 | (\x -> x < 256)`
- Compile-time-known values checked against predicates during reduction; runtime values generate guard code
- Longer term: explore full dependent types where type expressions can reference values (natural fit since types are already milang expressions evaluated in the type domain)

## Security & Capability Restriction

Designed but not yet implemented. Two attack surfaces to address: FFI escape (URL-imported modules calling arbitrary C) and unrestricted world capabilities.

### FFI Trust Zones
- Track trust zone (`Local` vs `Remote`) in the import resolver (`ResCtx`)
- `--no-remote-ffi` flag: block `.h` imports from URL-imported modules (transitive)
- `--no-ffi` flag: block all C imports regardless of trust zone

### Per-Import Capability Grants (`import'` whitelist)
When `--no-remote-ffi` is active, trusted packages (e.g. stdlib extensions that wrap C arrays or IO) can be explicitly opted in via the existing `import'` options record:

```milang
Array = import' "https://milang-std.example.com/array.mi" ({
  sha256 = "a1b2c3..."
  capabilities = ["ffi"]
})
```

- `capabilities` is an opt-in list; currently only `"ffi"` is defined
- The grant covers direct `.h` imports made by that module, but **not** further milang modules it imports (those still inherit the Remote trust zone)
- The trust is tied to the content hash — you're granting FFI to a specific pinned version, not a URL prefix
- `--no-ffi` overrides all grants (hard disable for fully sandboxed execution); `--no-remote-ffi` is the softer policy that `capabilities` can override
- A CLI convenience flag `--trust-ffi <url-prefix>` could be added later for development use, but `import'` is the principled production mechanism
- Note: world capabilities (`fs.read`, `io`, etc.) don't need a grant mechanism here — they're runtime values passed by the caller, so access control is already handled by what the calling code chooses to pass

### Capability Restriction Flags
- Granular flags: `--no-console`, `--no-fs-read`, `--no-fs-write`, `--no-fs`, `--no-exec`, `--no-env`
- `--sandbox` implies all of the above plus `--no-ffi` (only argv + pure computation + exit)
- Implementation: build restricted `world` records in the C runtime based on flags; restricted capabilities become stubs that are noops

## Compilation Targets

### WASM/JS
- Already proven working: `emcc` compiles milang's C output and runs in Node.js with zero source changes
- Needs: `%ld` → `%lld` fix for wasm32, auto-imply `--no-exec --no-fs --no-env`
- Output sizes observed: ~107–121 KB wasm + ~155 KB JS glue (unoptimized)

### CUDA
- Emit `.cu` file, compile with `nvcc` instead of `gcc`
- Current C output is already valid C++
- Auto-detect: if any import is `.cuh`/`.cu`/`.cpp`, switch to C++ compilation

## Concurrency

### Pure Parallelism
- `parallel : [~a] -> [a]` — fan out thunks across a thread pool, collect results
- Needs atomic refcounting in the runtime (`__atomic_fetch_add`/`__atomic_fetch_sub`)

### Async IO
- `spawn`, `await`, channels (`chan`, `send`, `recv`)
- World-splitting: spawned tasks inherit only explicitly granted capabilities
- `world.concurrent` as a gatable capability

## Standard Library Extensions

Importable modules (`.mi` files, not baked into the compiler) that extend the prelude for common use cases.

### Boosted Prelude (`std/prelude.mi`)
Functions missing from the current prelude that are useful in almost every program:
- `flatMap / concatMap` — map then flatten one level
- `foldRight` — right-associative fold (current `fold` is left)
- `scanLeft` / `scanRight` — fold emitting intermediate accumulator values
- `unfold` — generate a list from a seed (`(b -> Maybe (a, b)) -> b -> [a]`)
- `groupBy`, `sortBy`, `nubBy` — grouping/deduplication with custom equality
- `partition` — split a list into passing/failing by predicate
- `iterate` — `iterate f x = [x, f x, f (f x), ...]`
- `curry` / `uncurry` — convert between `(a -> b -> c)` and `({a; b} -> c)`
- `on` — `f \`on\` g = \\a b -> f (g a) (g b)` (useful with `sortBy`/`groupBy`)
- `maybe` / `either` — eliminator functions for `Maybe` and a future `Result` type

### Array Module (`std/array.mi`)
Flat, index-addressable arrays backed by C arrays (FFI). The current cons-cell list is unsuitable for indexed/numeric workloads:
- `Array.new :: Int -> a -> Array a` — allocate fixed-size array filled with a value
- `Array.fromList`, `Array.toList`
- `Array.get :: Array a -> Int -> Maybe a`, `Array.set :: Array a -> Int -> a -> Array a`
- `Array.map`, `Array.fold`, `Array.slice`
- Backed by a thin C header (`mi_array.h`) with arena-allocated `MiVal[]`

### String & Rope Module (`std/string.mi`)
The current string representation is `{data, len}` (immutable, C-string-backed). Useful additions:
- `String.split :: Str -> Str -> List Str` — split on delimiter
- `String.trim`, `String.trimLeft`, `String.trimRight`
- `String.startsWith`, `String.endsWith`, `String.contains`
- `String.replace :: Str -> Str -> Str -> Str`
- `String.toUpper`, `String.toLower`
- `String.toChars :: Str -> List Str`, `String.fromChars :: List Str -> Str`
- Rope type for efficient repeated concatenation (linked list of string segments, flattened lazily)

### Chunked IO (`std/io.mi`)
The current `world.fs` API reads/writes whole files as strings. For large files:
- `world.fs.readLines :: Str -> List Str` — stream file as lazy list of lines (thunks)
- `world.fs.withReader :: Str -> (Reader -> a) -> a` — bracket-style handle
- `Reader.readChunk :: Reader -> Int -> Maybe Str` — read N bytes
- `Reader.readLine :: Reader -> Maybe Str`
- `world.fs.withWriter`, `Writer.write`, `Writer.writeLine`, `Writer.flush`
- Backed by `FILE*` / `fread`/`fwrite` in the C runtime; handles closed automatically at scope exit

## Other Ideas
- Escape analysis for closure allocation optimization
- Generational arena (nursery + long-lived) for world IO closures
- Jupyter kernel for notebooks
