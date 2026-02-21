# Security & Capabilities

<!-- STUB: Cover the security model.

  ## Capability-based design
  - All IO flows through the `world` record
  - Functions only have access to capabilities explicitly passed to them
  - No global mutable state, no ambient authority
  
  ## Structural restriction
  ```
  -- greet only has access to IO, cannot exec or access filesystem
  greet io = io.println "hello"
  main world = greet world.io
  ```
  
  ## Planned CLI flags (not yet implemented)
  - `--no-console` — disable println, print, readLine
  - `--no-fs-read` — disable readFile, exists
  - `--no-fs-write` — disable writeFile, appendFile, remove
  - `--no-fs` — disable all filesystem
  - `--no-exec` — disable exec
  - `--no-env` — disable getEnv
  - `--no-ffi` — disallow all C imports
  - `--no-remote-ffi` — disallow C imports from URL-imported modules
  - `--sandbox` — all of the above (only argv + pure computation + exit)
  
  ## FFI trust zones
  - Local .mi files: FFI allowed
  - URL-imported .mi files: FFI allowed by default, restricted with --no-remote-ffi
  - Trust is transitive: if A imports URL B which imports relative C,
    C is in the remote trust zone
  
  ## World-taint tracking (auto-monad spine)
  - The compiler tracks which expressions reference world (directly or transitively)
  - Impure expressions are never eliminated during optimization
  - Impure expressions execute in declaration order
  - Pure expressions can be reordered/parallelized (future)
  
  ## Design principle
  Milang's security is structural: if a function doesn't receive world,
  it physically cannot do IO. There's no unsafePerformIO escape hatch.
  The only way to break this is C FFI — hence the FFI gating flags.
  
  Reference: tests/world_basic.mi
-->
