# Metaprogramming

<!-- STUB: Cover quote/unquote metaprogramming.

  ## Quote
  - `#expr` — captures the AST of expr as a record structure
  - Does NOT evaluate the expression
  - Returns a tagged record representing the syntax tree
  
  ## Splice
  - `$expr` — evaluates a quoted AST back into code
  - The expression must evaluate to a valid AST record
  
  ## AST record types
  Quoted expressions produce tagged records:
  - `#42` → `Int {val = 42}`
  - `#"hello"` → `Str {val = "hello"}`
  - `#x` → `Var {name = "x"}`
  - `#(f x)` → `App {fn = Var {name = "f"}, arg = Var {name = "x"}}`
  - `#(a + b)` → `Op {op = "+", left = ..., right = ...}`
  - `#(\x -> x)` → `Fn {param = "x", body = ...}`
  - `#{x = 1}` → `Rec {tag = "", fields = [...]}`
  
  ## Use cases
  - Code generation
  - DSL construction
  - Compile-time code transformation (via partial evaluation)
  
  ## Example
  ```
  -- Quote captures syntax
  ast = #(1 + 2)
  -- ast = Op {op = "+", left = Int {val = 1}, right = Int {val = 2}}
  
  -- Splice evaluates it back
  result = $ast    -- 3
  ```
  
  Reference: tests/metaprogramming.mi
-->
