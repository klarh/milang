# Values & Literals

<!-- STUB: Cover all literal types with examples.
  
  ## Integers
  - `42`, `0`, `-3` (negation is `0 - 3`, since there's no unary minus)
  - Arbitrary precision at compile time, int64_t at runtime
  
  ## Floats
  - `3.14`, `0.5` — must have digits on both sides of the dot
  - Compiled to C double
  
  ## Strings
  - `"hello"` — double-quoted, supports escape sequences: \n \t \\ \"
  - Triple-quoted strings with margin stripping:
    ```
    msg = """
      Hello, world!
        indented
      """
    ```
    The closing """ indentation defines the margin (Swift-style).
    Everything to the left of that is stripped. Produces: "Hello, world!\n  indented"
  
  ## Lists
  - `[1, 2, 3]` — desugars to Cons/Nil linked list records
  - `[]` is Nil
  - `1 : [2, 3]` — cons operator (right-associative)
  - Lists are NOT arrays — they're Cons-cell linked lists
  - `List = {Nil; Cons head tail}` is declared in the prelude
  
  ## Booleans
  - No dedicated boolean type — uses integers: 1 = true, 0 = false
  - Comparison operators return 1 or 0
  - `if` checks truthiness (0 = false, non-zero = true)
  - `&&` and `||` short-circuit and return 1 or 0
  
  ## Records (see Records & ADTs chapter)
  ## Functions (see Functions chapter)
  
  Reference: tests/strings.mi, tests/lists.mi, tests/arithmetic.mi
-->
