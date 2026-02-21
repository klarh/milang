# Type Annotations (::)

<!-- STUB: Cover the type annotation system.

  ## Syntax
  ```
  name :: typeExpr
  ```
  - Type annotations are separate bindings that merge with value bindings
  - `:` in type expressions means "function type" (right-associative)
  
  ## Primitive types
  - `Num` — integers
  - `Float` — floating point
  - `Str` — strings
  - `List` — linked list
  
  ## Examples
  ```
  double :: Num : Num
  double x = x * 2
  
  add :: Num : Num : Num
  add a b = a + b
  
  Point :: {x = Num; y = Num}
  mkPoint :: Num : Num : Point
  mkPoint x y = {x; y}
  ```
  
  ## Polymorphism
  - Unbound names in type expressions are type variables:
    ```
    map :: (a : b) : List a : List b
    apply :: (a : b) : a : b
    ```
  
  ## Type checking behavior
  - Currently bidirectional: pushes :: annotations down, infers bottom-up
  - Type errors are reported as warnings (not hard errors yet)
  - The type checker uses structural compatibility, not nominal typing
  - Records match by shape: {x = Num; y = Num} matches any record with those fields
  
  ## Key property
  `:` is overloaded by domain:
  - Value domain: cons operator (`1 : [2, 3]`)
  - Type domain: function type (`Num : Num : Bool`)
  This works because `::` clearly delineates the boundary.
  
  Reference: tests/types.mi, tests/type_errors.mi
-->
