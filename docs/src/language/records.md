# Records & ADTs

Records are milang's primary data structure. They hold named `fields`, support
structural updates, and form the basis of algebraic data types (ADTs).

## Record Literals

A record is a set of `field = value` pairs inside braces, separated by `;` or
newlines:

```milang,run
point = {x = 3; y = 4}
person = {name = "Alice"; age = 30}
```

## Field Access

Use dot notation to read a field. Dots chain for nested records.

```milang,run
point = {x = 3; y = 4}
a = point.x
b = point.y
```

## Positional Access

Fields can also be accessed by declaration order using `_0`, `_1`, etc.:

```milang,run
pair = {first = "hello"; second = "world"}
a = pair._0
b = pair._1
```

## Record Update

The `<-` operator creates a new record with selected `fields` replaced. Fields
not mentioned are carried over unchanged.

```milang,run
base = {x = 1; y = 2; z = 3}
moved = base <- {x = 10; z = 30}
```

## Destructuring

Bind `fields` from a record directly into the current scope. Use `{field}` for
same-name bindings, or `{local = field}` to rename:

```milang,run
point = {x = 3; y = 4}
{x; y} = point
sum = x + y
```

Renaming during destructuring:

```milang,run
point = {x = 3; y = 4}
{myX = x; myY = y} = point
result = myX + myY
```

## Scope-as-Record

When a function body has no explicit result expression — just indented
bindings — the named bindings are collected into an implicit record:

```milang,run
makeVec x y =
  magnitude = x + y
  product = x * y
v = makeVec 3 4
```

Bare expressions (not bound to a name) execute for their side effects and are
**not** included in the returned record. This is how `main` works — see the
[Scopes](scopes.md) chapter.

## ADTs (Algebraic Data Types)

An uppercase name bound to braces containing uppercase constructors declares a
tagged union:

```milang,run
Shape = {Circle radius; Rect width height; Point}
c = Circle 5
r = Rect 3 4
p = Point
```

Each constructor becomes a function that produces a tagged record. Zero-field
constructors (like `Point` above) are plain tagged records with no arguments.

Constructors are also available namespaced under the type name
(e.g. `Shape.Circle`).

## Constructors as Functions

Because constructors are just functions, they work naturally with `map` and
other higher-order functions:

```milang,run
values = map (\x -> Just x) [1, 2, 3]
```

## Pattern Matching on Tags

Use the `->` operator to match on a value's constructor `tag`. After a `tag`
matches, the record's `fields` are accessible via dot notation or destructuring:

```milang,run
Shape = {Circle radius; Rect width height}
area shape = shape ->
  Circle = 3.14 * shape.radius * shape.radius
  Rect = shape.width * shape.height
a = area (Circle 5)
b = area (Rect 3 4)
```

Named-field destructuring in alternatives:

```milang,run
Shape = {Circle radius; Rect width height}
area shape ->
  Circle {radius} = 3.14 * radius * radius
  Rect {width; height} = width * height
a = area (Circle 5)
b = area (Rect 3 4)
```

See the [Pattern Matching](pattern-matching.md) chapter for the full range of
patterns, guards, and list matching.

## Record Introspection

Several built-in functions let you inspect records dynamically:

| Function | Returns |
|----------|---------|
| `fields r` | List of `{name, value}` records (`[]` for non-records) |
| `fieldNames r` | List of field-name strings |
| `tag r` | Constructor `tag` string, or `""` for untagged values |
| `getField r "name"` | `Just value` if present, `Nothing` if missing |
| `setField r "name" val` | New record with field set |
