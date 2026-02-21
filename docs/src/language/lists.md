# Lists

<!-- STUB: Cover list syntax and prelude functions.

  ## Implementation
  - Lists are Cons-cell linked lists, NOT arrays
  - `List = {Nil; Cons head tail}` is declared in the prelude
  - `[1, 2, 3]` desugars to `Cons 1 (Cons 2 (Cons 3 Nil))`
  - Smart printing: Cons/Nil chains display as `[1, 2, 3]`
  
  ## Constructing
  - `[]` — empty list (Nil)
  - `[1, 2, 3]` — list literal
  - `1 : [2, 3]` — cons operator (right-associative)
  - `range 1 5` — `[1, 2, 3, 4, 5]`
  
  ## Accessing
  - `head xs` — first element
  - `tail xs` — everything after first
  - `last xs` — last element
  - `init xs` — everything except last
  - `get 2 xs` — element at index 2
  - `len xs` — length
  
  ## Transforming
  - `map f xs` — apply f to each element
  - `filter f xs` — keep elements where f returns truthy
  - `fold f acc xs` — left fold
  - `reverse xs`
  - `sort xs` / `sortBy f xs`
  - `take n xs` / `drop n xs`
  - `zip xs ys` — pair up elements
  - `enumerate xs` — list of {index, value} records
  
  ## Combining
  - `concat xs ys` — append two lists
  - `push xs x` — append single element
  - `join sep xs` — join strings with separator
  
  ## Querying
  - `any f xs` / `all f xs` — predicate checks
  - `contains x xs` — membership test
  - `sum xs` / `product xs` — numeric aggregation
  
  ## Pattern matching on lists
  - `[a, b, c]` — match exact length
  - `[first, ...rest]` — match head + tail
  - `[]` — match empty list (matches Nil tag)
  
  Reference: tests/lists.mi, tests/list_patterns.mi, tests/prelude.mi
-->
