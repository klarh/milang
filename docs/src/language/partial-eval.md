# Partial Evaluation

<!-- STUB: Cover the core compilation model.

  ## What is partial evaluation?
  - The compiler evaluates your program as far as possible at compile time
  - Expressions with all-known inputs reduce to literals
  - Expressions with unknown inputs (runtime values) stay as residual code
  - This IS the optimizer — there's no separate optimization pass
  
  ## Examples
  ```
  -- Fully reduced at compile time:
  x = 6 * 7              -- becomes: x = 42
  f x = x * 2; y = f 21  -- becomes: y = 42
  
  -- Partially reduced:
  double x = x * 2        -- stays as function (x is unknown)
  
  -- Runtime-dependent:
  main world =
    line = world.io.readLine     -- can't reduce (depends on runtime)
    world.io.println line        -- stays as residual code
  ```
  
  ## How it works internally
  - Uses `reduceD` recursive descent with depth limit (128)
  - SCC (strongly connected components) analysis for binding dependencies
  - Concrete values: IntLit, FloatLit, StringLit, Lam, Record with concrete fields
  - Non-concrete (residual): App, BinOp, FieldAccess, etc.
  - Recursive functions: depth-limited unrolling with concrete-args gate
  
  ## Implications
  - Abstraction is free when fully known at compile time
  - Configuration, constants, type-level computations → zero runtime cost
  - The compiler output is "pre-evaluated" — only dynamic code remains
  
  ## dump command
  ```
  ./milang dump file.mi    -- shows the reduced AST (what codegen sees)
  ```
  
  Reference: tests/partial_eval.mi, any test file (use `milang dump` to see reduction)
-->
