# Pattern Matching

<!-- STUB: Cover pattern matching in depth.

  ## Basic syntax
  - `expr ->` introduces match alternatives
  - Alternatives can be inline (`;`-separated) or indented
  
  ## Inline alternatives
  ```
  classify x = x -> 0 = "zero"; 1 = "one"; _ = "other"
  ```
  
  ## Indented alternatives
  ```
  classify x = x ->
    0 = "zero"
    1 = "one"
    _ = "other"
  ```
  
  ## Pattern types
  - Literal: `42`, `"hello"` — exact value match
  - Variable: `x` — matches anything, binds the value to x
  - Wildcard: `_` — matches anything, doesn't bind
  - Constructor: `Circle`, `Rect` — matches by tag name
    - After matching, fields accessible via `s.radius`, `s.width`, etc.
  - List: `[a, b, c]` — matches list of exact length
  - List with spread: `[first, ...rest]` — matches at least one element
  - Nested: `Cons` then check `s.head`, `s.tail`
  
  ## Guards
  - `| condition = body` — guard must be true for the alt to match
  ```
  abs x = x ->
    n | n >= 0 = n
    n = 0 - n
  ```
  
  ## Constructor patterns
  ```
  area s = s ->
    Circle = 3.14 * s.radius * s.radius
    Rect = s.width * s.height
  ```
  - After matching `Circle`, the scrutinee `s` has fields `radius`
  - After matching `Rect`, the scrutinee `s` has fields `width`, `height`
  
  ## Named constructor fields
  ```
  Shape = {Circle {radius}; Rect {width; height}}
  
  area s = s ->
    Circle {radius} = 3.14 * radius * radius
    Rect {width; height} = width * height
  ```
  
  Reference: tests/pattern_matching.mi, tests/guards.mi, tests/adt.mi,
             tests/list_patterns.mi
-->
