# Thunks & Laziness

<!-- STUB: Cover thunks and lazy evaluation.

  ## Thunks
  - `~expr` — creates a thunk (delayed evaluation)
  - The expression is NOT evaluated until the thunk is forced
  - Thunks are forced automatically when their value is needed
  
  ## Lazy bindings
  - `name := expr` — lazy binding (creates a thunk)
  - Evaluated at most once, result cached after first force
  - Useful for expensive computations that might not be needed
  
  ## if requires thunks
  - `if` is a function, not syntax — both branches would be eagerly evaluated
  - Solution: pass branches as thunks
  ```
  if (x > 0) ~x ~(0 - x)
  ```
  - `~x` and `~(0 - x)` are thunks — only one gets forced
  
  ## Use cases
  - Conditional branches (if)
  - Expensive computations (lazy binding)
  - Infinite structures (with recursive lazy bindings)
  - Short-circuit evaluation (&&, || use thunks internally)
  
  Reference: tests/thunks.mi, tests/lazy.mi
-->
