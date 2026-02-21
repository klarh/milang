# Traits & Effects (:~)

<!-- STUB: Cover the traits/effects annotation domain.

  ## Syntax
  ```
  name :~ traitsExpr
  ```
  - Traits expressions are lists of effect names
  - Effect names are just milang names/field accesses
  
  ## Effect names
  - `console` — println, print, readLine
  - `fs.read` — readFile, exists
  - `fs.write` — writeFile, appendFile, remove
  - `exec` — process execution
  - `env` — environment variable access
  
  ## Special values
  - `pure` = `[]` — no effects (pure function)
  - User-defined groups via assignment:
    ```
    readonly :~ [console, fs.read]
    readwrite :~ [console, fs.read, fs.write]
    ```
  
  ## Examples
  ```
  add :~ pure
  add a b = a + b
  
  greet :~ [console]
  greet world = world.io.println "hello"
  
  server :~ [console, fs.read, fs.write]
  server world = ...
  ```
  
  ## How it works
  - Traits annotations are parsed and stored on bindings (bindTraits field)
  - Currently informational — the compiler tracks impurity via the auto-monad
    spine (world-taint analysis) independently
  - Future: the compiler will verify that inferred effects ⊆ declared effects
  
  ## Connection to auto-monad spine
  - The reducer tracks which bindings are "world-tainted" (impure)
  - Impure bindings are guaranteed to execute in declaration order
  - Pure bindings can float freely
  - Traits annotations will eventually be verified against this analysis
  
  Reference: tests/traits.mi
-->
