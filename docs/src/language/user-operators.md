# User-Defined Operators

<!-- STUB: Cover defining custom operators.

  ## Defining operators
  - Wrap operator in parens to define it as a function:
    ```
    (<+>) a b = {x = a.x + b.x; y = a.y + b.y}
    ```
  - Operator characters: + - * / ^ < > = ! & | @ % ? :
  
  ## Setting precedence
  - Use parse domain :! to declare precedence and associativity:
    ```
    (<+>) :! {prec = 6; assoc = Left}
    ```
  - Must appear before first use
  
  ## Using operators
  - Infix: `a <+> b`
  - Prefix: `(<+>) a b`
  
  ## Using functions as operators
  - Backtick syntax: `3 \`add\` 4` = `add 3 4`
  
  ## Operator as function
  - `(+)` — wraps built-in + as a function value
  - Useful for: `fold (+) 0 [1,2,3]`
  
  Reference: tests/user_operators.mi, tests/operators.mi
-->
