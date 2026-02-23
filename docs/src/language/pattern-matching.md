# Pattern Matching

Pattern matching in milang uses the `->` operator to dispatch on a value's
shape. There are no keywords — `->` is an expression that evaluates the first
alternative whose pattern matches.

## Basic Syntax

Write `expr ->` followed by alternatives. Each alternative is
`pattern = body`. Alternatives can appear inline (separated by `;`) or
indented on separate lines.

Inline:

```milang,run
classify x = x -> 0 = "zero"; 1 = "one"; _ = "other"
a = classify 0
b = classify 1
c = classify 42
```

Indented:

```milang,run
classify x = x ->
  0 = "zero"
  1 = "one"
  _ = "other"
a = classify 0
b = classify 1
c = classify 42
```

## Literal Patterns

Integers and strings match by exact value:

```milang,run
describe n = n ->
  0 = "zero"
  1 = "one"
  _ = "many"
a = describe 0
b = describe 1
c = describe 99
```

## Variable Patterns

A lowercase name matches any value and binds it for use in the body:

```milang,run
myAbs x = x ->
  n | n >= 0 = n
  n = 0 - n
a = myAbs 5
b = myAbs (0 - 3)
```

## Wildcard

`_` matches any value without binding it. Use it for catch-all branches:

```milang,run
isZero x = x ->
  0 = 1
  _ = 0
a = isZero 0
b = isZero 7
```

## Constructor Tag Patterns

Match on a tagged record's constructor. After matching, the scrutinee's `fields`
are accessible through dot notation:

```milang,run
Shape = {Circle radius; Rect width height}
area shape = shape ->
  Circle = 3.14 * shape.radius * shape.radius
  Rect = shape.width * shape.height
a = area (Circle 5)
b = area (Rect 3 4)
```

With named-field destructuring in the pattern, `fields` are bound directly:

```milang,run
Shape = {Circle radius; Rect width height}
area shape ->
  Circle {radius} = 3.14 * radius * radius
  Rect {width; height} = width * height
a = area (Circle 5)
b = area (Rect 3 4)
```

## List Patterns

Match a list by its elements. `[a, b, c]` matches a list of exactly three
elements. `[first, ...rest]` matches one or more elements, binding the `tail`:

```milang,run
xs = [10, 20, 30, 40]
result = xs ->
  [a, b, ...rest] = {first = a; second = b; rest = rest}
  [] = {first = 0; second = 0; rest = []}
```

An empty-list pattern matches `[]` (`Nil`):

```milang,run
isEmpty xs = xs ->
  [] = "empty"
  _ = "non-empty"
a = isEmpty []
b = isEmpty [1]
```

## Guards

A guard adds a condition to an alternative using `| condition` before the `=`.
The alternative only matches when both the pattern and the guard are satisfied:

```milang,run
classify x = x ->
  n | n < 0 = "negative"
  n | n == 0 = "zero"
  _ = "positive"
a = classify (0 - 5)
b = classify 0
c = classify 10
```

## Guard-Only Matching

When every alternative uses only a guard (no structural pattern), you can
write guards directly after `->`:

```milang,run
classify x = x ->
  | x < 0 = "negative"
  | x == 0 = "zero"
  | _ = "positive"
a = classify (0 - 5)
b = classify 0
c = classify 10
```

## Combined Pattern + Guard

A constructor or literal pattern can be paired with a guard:

```milang,run
Shape = {Circle radius; Rect width height}
safeArea shape ->
  Circle {radius} | radius > 0 = 3.14 * radius * radius
  _ = 0
a = safeArea (Circle 5)
b = safeArea (Circle 0)
c = safeArea (Rect 3 4)
```

## Match in Function Bindings

The `f param ->` sugar defines a function that immediately matches its `last`
parameter, avoiding an extra `= param ->` layer:

```milang,run
Shape = {Circle radius; Rect width height}
describe label shape ->
  Circle = label + ": circle"
  Rect = label + ": rect"
  _ = label + ": unknown"
a = describe "shape" (Circle 5)
b = describe "shape" (Rect 3 4)
```

## Exhaustiveness Tips

Milang does not enforce exhaustive patterns at compile time. Always include a
wildcard `_` or variable catch-all as the `last` alternative to avoid runtime
match failures. Constructor-heavy code benefits from a final `_ = ...` branch
as a safety net.

## Matching Maybe

```milang,run
matchMaybe m = m ->
  Just {val} = "Just(" + toString val + ")"
  Nothing = "Nothing"

main world =
  world.io.println (matchMaybe (Just 5))
  world.io.println (matchMaybe Nothing)
```
