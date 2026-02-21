# Operators

<!-- STUB: Cover built-in and user-defined operators.

  ## Arithmetic
  - `+` `-` `*` `/` `%` `**` — standard math ops
  - `/` is integer division for ints, float division for floats
  - `%` is modulo (ints only)
  - `**` is power
  - `+` also does string concatenation (and `++` is an alias)
  
  ## Comparison
  - `==` `/=` `<` `>` `<=` `>=`
  - Return 1 (true) or 0 (false)
  - `==` and `/=` work structurally on records, lists, strings
  
  ## Logical
  - `&&` — short-circuit AND
  - `||` — short-circuit OR  
  - `not` — logical negation (it's a function, not an operator)
  - All return 1 or 0
  
  ## List
  - `:` — cons (right-associative): `1 : 2 : []` = `[1, 2]`
  
  ## String
  - `+` and `++` — concatenation
  
  ## Operator as function / function as operator
  - `(+) 3 4` — wrap in parens to use prefix
  - `3 \`add\` 4` — backtick syntax for infix function application
  
  ## Precedence and associativity
  - Default precedence for user operators can be set via parse domain :!
  - See Parse Declarations chapter for details
  - Built-in precedence roughly follows Haskell conventions
  
  Reference: tests/arithmetic.mi, tests/comparison.mi, tests/operators.mi,
             tests/user_operators.mi
-->
