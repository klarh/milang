# Type Annotations (`::`)

Type annotations in milang are optional — the compiler infers types. When you do annotate, you use the `::` domain to attach a type expression to a binding. Annotations are separate lines that merge with the corresponding value binding.

## Syntax

```milang
name :: typeExpr
name args = body
```

Inside a type expression, `:` means "function type" and is right-associative. So `Num : Num : Num` means "a function that takes a `Num`, then a `Num`, and returns a `Num`."

## Primitive Types

| Type | Description |
|------|-------------|
| `Num` | Alias for `Int` (backward compatibility) |
| `Int` | Signed 64-bit integer (alias for `Int' 64`) |
| `UInt` | Unsigned 64-bit integer (alias for `UInt' 64`) |
| `Float` | 64-bit floating-point (alias for `Float' 64`) |
| `Byte` | Unsigned 8-bit integer (alias for `UInt' 8`) |
| `Str` | String |
| `List` | Linked list (Cons/`Nil`) |

### Sized Numeric Types

The primitive type constructors `Int'`, `UInt'`, and `Float'` take a compile-time bit width:

```milang
add8 :: Int' 8 : Int' 8 : Int' 8
add8 a b = a + b

compact :: Float' 32 : Float' 32
compact x = x * 1.0
```

The prelude defines convenient aliases:

```milang
Int = Int' 64
UInt = UInt' 64
Float = Float' 64
Byte = UInt' 8
```

You can define your own aliases:

```milang
Short = Int' 16
Word = UInt' 32
```

## Basic Examples

```milang,run
double :: Num : Num
double x = x * 2

add :: Num : Num : Num
add a b = a + b

greeting :: Str : Str
greeting name = "Hello, " + name

result = add (double 3) 4
message = greeting "milang"
```

## Record Types

Record types describe the shape of a record — field names and their types:

```milang
Point :: {x = Num; y = Num}
```

You can use a named record type in function signatures:

```milang,run
Point :: {x = Num; y = Num}

mkPoint :: Num : Num : Point
mkPoint x y = {x = x; y = y}

p = mkPoint 3 4
```

## Polymorphism (Type Variables)

Any unbound identifier in a type expression is automatically a type variable. There is no `forall` keyword — just use lowercase names:

```milang
apply :: (a : b) : a : b
apply f x = f x
```

Here `a` and `b` are type variables. `apply` works for any function type `a : b` applied to an `a`, producing a `b`.

```milang,run
apply :: (a : b) : a : b
apply f x = f x

double x = x * 2
result = apply double 21
```

## ADT Types

You can annotate functions that operate on algebraic data types:

```milang,run
Shape = {Circle radius; Rect width height}

area :: Shape : Num
area s = s ->
  Circle = 3 * s.radius * s.radius
  Rect = s.width * s.height

a = area (Circle 5)
b = area (Rect 3 4)
```

## The Dual Meaning of `:`

The `:` symbol is overloaded depending on context:

- **Value domain:** cons operator — `1 : [2, 3]` builds a list
- **Type domain:** function arrow — `Num : Num : Num` describes a function

This works because `::` on its own line clearly marks the boundary between value code and type code. There is never ambiguity.

## Type Checking Behavior

The type checker is bidirectional: it pushes `::` annotations downward and infers types bottom-up. Type errors are currently reported as warnings rather than hard errors. Checking is structural — records match by shape (field names and types), not by name. Any record with the right `fields` satisfies a record type.
