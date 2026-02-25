# Traits & Effects (`:~`)

The `:~` annotation domain attaches trait or effect information to a binding. It describes *what capabilities* a function uses — console IO, file reads, process execution, and so on. Traits annotations are orthogonal to type annotations (`::`) and can coexist on the same binding.

## Syntax

```milang
name :~ traitsExpr
```

The traits expression is typically a list of effect names:

```milang
greet :~ [console]
greet world = world.io.println "hello"
```

## Effect Names

| Effect | Capabilities covered |
|--------|---------------------|
| `console` | `println`, `print`, `readLine` |
| `fs.read` | `readFile`, `exists` |
| `fs.write` | `writeFile`, `appendFile`, `remove` |
| `exec` | `process.exec` |
| `env` | `getEnv` |

Use `[]` (empty list) or define a name bound to `[]` to declare a function as pure:

```milang
pure :~ []

add :~ pure
add a b = a + b
```

## Defining Effect Groups

You can define reusable groups of effects:

```milang
readonly :~ [console, fs.read]
readwrite :~ [console, fs.read, fs.write]
```

Then reference those groups in other annotations.

## Example

```milang,run
distance :~ []
distance x1 y1 x2 y2 = (x2 - x1)**2 + (y2 - y1)**2

main world =
  world.io.println (distance 0 0 3 4)
```

## Combining with Other Domains

All annotation domains can coexist on a single binding:

```milang
add :? "Add two numbers"
add :: Num : Num : Num
add :~ []
add a b = a + b
```

## Current Status

Trait annotations are parsed, stored, and **enforced** by the compiler. The compiler performs taint analysis: it tracks the `world` value and any names that transitively reference it (via aliasing or closures), then infers the effect set of every binding. If a function's inferred effects are not a subset of its declared traits, the compiler emits an error.

**Functions without a `:~` annotation are assumed pure** (`:~ []`). This means any function that performs IO must declare its effects. The only exception is `main`, which is implicitly granted all capabilities.

For example, declaring `:~ []` (pure) but calling `world.io.println` inside the body is a compile error — and so is omitting the annotation entirely:

```milang
-- This is an error: no annotation, so assumed pure, but uses console
helper world = world.io.println "oops"

-- Fix: add trait annotation
helper :~ [console]
helper world = world.io.println "ok"
```
