# Functions

<!-- STUB: Cover function definition and application.

  ## Definition
  - `f x y = x + y` — name, params, =, body
  - Parameters are space-separated before the =
  - Body is a single expression (or a scope with indented children)
  - Functions are first-class values (closures)
  
  ## Application
  - Juxtaposition: `f 3 4` — left-associative, like Haskell
  - All functions are curried: `add 3` returns a function that adds 3
  - Partial application is natural: `double = map (\x -> x * 2)`
  
  ## Lambdas
  - `\x -> x + 1` — single param
  - `\x y -> x + y` — multi param
  - Can be used anywhere an expression is expected
  
  ## Pipe and Composition
  - `x |> f` desugars to `f x` — pipe operator for readability
  - `f >> g` — compose left-to-right: `\x -> g (f x)`
  - `f << g` — compose right-to-left: `\x -> f (g x)`
  - Example: `[1,2,3] |> map (\x -> x*2) |> filter (\x -> x > 2) |> sum`
  
  ## Recursion
  - Direct recursion: just reference the function name in its body
  - Mutual recursion: works automatically (SCC-based cycle detection)
  - TCO (tail-call optimization): self-calls and mutual calls in tail position
    use goto-based trampolining — no stack overflow
  
  ## if
  - `if` is a function, not a keyword: `if condition ~thenBranch ~elseBranch`
  - The branches are thunks (~expr) to prevent eager evaluation
  - Example: `if (x > 0) ~x ~(0 - x)`
  
  Reference: tests/functions.mi, tests/closures.mi, tests/recursion.mi,
             tests/pipes.mi, tests/tco.mi
-->
