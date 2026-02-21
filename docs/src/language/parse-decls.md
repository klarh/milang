# Parse Declarations (:!)

<!-- STUB: Cover operator precedence and associativity declarations.

  ## Syntax
  ```
  (op) :! {prec = N; assoc = Left}
  (op) :! {prec = N; assoc = Right}
  ```
  - Declares the precedence (integer) and associativity of a user-defined operator
  - Must appear before the operator is used in expressions
  
  ## How it works
  - The parser does a pre-scan of the source to collect :! declarations
  - These configure the operator precedence table
  - Higher prec = binds tighter
  - Left/Right determines grouping: `a op b op c`
    - Left: `(a op b) op c`
    - Right: `a op (b op c)`
  
  ## Built-in precedences (approximate)
  - `||` — 2
  - `&&` — 3
  - `==` `/=` — 4
  - `<` `>` `<=` `>=` — 4
  - `+` `-` `++` — 6
  - `*` `/` `%` — 7
  - `**` — 8 (right-associative)
  - `:` — 5 (right-associative, cons)
  - `|>` — 1 (left-associative, pipe)
  - `>>` `<<` — 1 (composition)
  
  ## Example
  ```
  (<+>) :! {prec = 6; assoc = Left}
  (<+>) a b = {x = a.x + b.x; y = a.y + b.y}
  
  result = {x=1;y=2} <+> {x=3;y=4}   -- {x=4, y=6}
  ```
  
  Reference: tests/user_operators.mi, tests/parse_domain.mi
-->
