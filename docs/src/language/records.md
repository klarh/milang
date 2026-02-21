# Records & ADTs

<!-- STUB: Cover records and algebraic data types.

  ## Anonymous Records
  - `{x = 3; y = 4}` — record literal
  - Fields separated by `;` or newlines
  - `point.x` — field access via dot notation
  - `point._0` — positional access (by declaration order)
  
  ## Record Updates
  - `point:{x = 10}` — creates new record with x changed, other fields kept
  - Syntax: `record:{field = newValue; ...}`
  
  ## Destructuring
  - `{x; y} = point` — binds x and y from record fields
  - `{myX = x} = point` — binds myX from field x (renaming)
  
  ## Scope-as-Record
  - Multi-line function bodies without an explicit body expression return an
    implicit record of their named bindings:
    ```
    makeVec x y =
      magnitude = sqrt (x**2 + y**2)
      sum = x + y
    -- makeVec 3 4 returns {magnitude = 5, sum = 7}
    ```
  - Bare expressions (not bound to a name) execute for effect only and are
    NOT included in the record
  
  ## ADTs (Algebraic Data Types)
  - Uppercase name with braces containing uppercase constructors:
    ```
    Shape = {Circle radius; Rect width height}
    ```
  - This creates constructor functions: `Circle 5` → tagged record
  - Constructors can have named fields: `{Circle {radius}; Rect {width; height}}`
  - Union declarations: constructors are also available as top-level names
  - Pattern match on constructors (see Pattern Matching chapter)
  
  ## Record Introspection
  - `fields r` — list of {name, value} records
  - `fieldNames r` — list of field name strings
  - `tag r` — constructor tag (e.g. "Circle") or "" for untagged
  - `getField r "name"` — dynamic field access by string
  - `setField r "name" val` — new record with field set
  
  Reference: tests/records.mi, tests/record_update.mi, tests/destructuring.mi,
             tests/adt.mi, tests/positional_fields.mi
-->
