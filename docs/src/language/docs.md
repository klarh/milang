# Documentation (:?)

<!-- STUB: Cover the documentation annotation domain.

  ## Syntax
  ```
  name :? docExpr
  ```
  - Doc expressions are regular milang expressions
  - Typically strings or structured records
  
  ## Simple string docs
  ```
  add :? "Add two numbers"
  add a b = a + b
  ```
  
  ## Structured docs
  ```
  distance :? {
    summary = "Euclidean distance between two points"
    params = {
      p1 = "First point"
      p2 = "Second point"
    }
    returns = "The distance as a float"
  }
  distance p1 p2 = sqrt ((p2.x - p1.x)**2 + (p2.y - p1.y)**2)
  ```
  
  ## Multi-line docs (triple-quoted strings)
  ```
  greet :? """
    Greet a person by name.
    Prints a friendly message to the console.
    Uses world.io.println internally.
    """
  greet world name = world.io.println ("Hello, " + name)
  ```
  
  ## Combining all domains
  ```
  distance :? "Euclidean distance"
  distance :: Point : Point : Num
  distance :~ pure
  distance p1 p2 = sqrt ((p2.x - p1.x)**2 + (p2.y - p1.y)**2)
  ```
  All five domains can coexist on a single binding.
  
  ## Future: `milang doc` command
  - Planned: extract :? annotations from source and generate reference docs
  - Could produce mdBook-compatible Markdown automatically
  
  Reference: tests/docs.mi
-->
