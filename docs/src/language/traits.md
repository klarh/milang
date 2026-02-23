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

Traits annotations are parsed and stored on bindings, but they are currently informational. The compiler independently tracks impurity through its auto-monad spine (`world`-taint analysis), which guarantees that impure expressions execute in declaration order. In the future the compiler will verify that a function's inferred effects are a subset of its declared traits.
