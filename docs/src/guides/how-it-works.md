# How Milang Works

<!-- STUB: Explain the compilation pipeline at a high level.
  
  Pipeline: source.mi → Parser → Import Resolver → Partial Evaluator → C Codegen → gcc
  
  ## Parser (Megaparsec)
  - Whitespace-sensitive (indentation for scopes, like Haskell/Python)
  - Produces AST: Expr type with variants like IntLit, App, Lam, With, Record, etc.
  - Five binding domains parsed: = :: :~ :? :!
  - No keywords — `if` is a function, pattern matching uses `->` operator
  
  ## Import Resolver
  - Resolves `import "file.mi"` and `import "url"` declarations
  - Handles circular imports (marks as lazy)
  - C header imports: parses .h files to extract function signatures
  - URL imports: downloads, verifies sha256 hash, caches
  
  ## Partial Evaluator (the heart)
  - Reduces the AST as far as possible given known values
  - Fully-known expressions become literals at compile time
  - Uses SCC (strongly connected components) for binding dependency analysis
  - Handles recursive functions via depth-limited unrolling
  - The "optimizer" IS the evaluator — no separate optimization pass
  - Example: `f x = x * 2; y = f 21` → `y = 42` at compile time
  
  ## C Code Generation
  - Emits a single .c file with an embedded runtime (~1600 lines)
  - Runtime: arena allocator, tagged MiVal union, environment chain, TCO via goto
  - Values: MI_INT, MI_FLOAT, MI_STRING, MI_RECORD, MI_CLOSURE, MI_NIL
  - Compiles with gcc (or emcc for WASM)
  
  ## Key insight
  Because the partial evaluator runs at compile time, abstractions that are
  fully known (constants, known function applications, configuration) have
  ZERO runtime cost. Only runtime-dependent code (IO, user input, dynamic
  dispatch) remains in the compiled output.
-->
