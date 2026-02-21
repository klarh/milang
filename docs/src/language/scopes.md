# Scopes & Bindings

<!-- STUB: Cover how scopes work — this changed recently.

  ## Basic bindings
  - `name = expr` — eager binding
  - `name := expr` — lazy binding (thunk)
  - `name params = expr` — function binding
  
  ## Scopes
  A scope is a list of bindings, created by indentation or braces.
  
  ## Two scope modes
  
  ### 1. Explicit body (expression after =)
  When a binding has an expression after `=` followed by indented children,
  the expression is the scope's return value and the children are local bindings:
  ```
  compute x = result
    doubled = x * 2
    result = doubled + 1
  -- compute 7 returns 15
  ```
  
  ### 2. Implicit record (no expression after =)
  When a binding has no expression after `=` (just indented children),
  the named bindings form an implicit record:
  ```
  makeVec x y =
    magnitude = sqrt (x**2 + y**2)
    sum = x + y
  -- makeVec 3 4 returns {magnitude = 5.0, sum = 7}
  ```
  
  ## Bare expressions (effects)
  Bare expressions in scopes (not bound to a name) execute for their
  side effects. Their results are discarded and NOT included in any
  implicit record:
  ```
  main world =
    world.io.println "hello"    -- effect, discarded
    world.io.println "world"    -- effect, discarded
    0                           -- this is the body (exit code)
  ```
  
  ## Inline scopes (braces)
  ```
  f x = result { doubled = x * 2; result = doubled + 1 }
  ```
  
  ## Binding order
  - Bindings evaluate left-to-right (top-to-bottom)
  - Later bindings can reference earlier ones
  - The compiler tracks impure bindings (world-tainted) and guarantees
    they execute in declaration order (auto-monad spine)
  - Pure bindings can theoretically be reordered by the optimizer
  
  Reference: tests/scopes.mi, tests/world_basic.mi
-->
