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
| `Int` | Arbitrary-precision signed integer (alias for `Int' 0`) |
| `UInt` | Arbitrary-precision unsigned integer (alias for `UInt' 0`) |
| `Float` | 64-bit floating-point (alias for `Float' 64`) |
| `Byte` | Unsigned 8-bit integer (alias for `UInt' 8`) |
| `Str` | String |
| `List` | Linked list (Cons/`Nil`) |

### Sized Numeric Types

The constructors `Int'`, `UInt'`, and `Float'` take a compile-time bit width
and serve as both **type annotations** and **value constructors**:

```milang
-- as type annotations
add8 :: Int' 8 : Int' 8 : Int' 8
add8 a b = a + b

-- as value constructors
x = Int' 8 42      -- signed 8-bit integer with value 42
y = UInt' 16 1000   -- unsigned 16-bit integer
z = Int 42          -- arbitrary-precision integer (Int = Int' 0)
```

The prelude defines convenient aliases (you can define your own too):

```milang
Int = Int' 0       -- arbitrary precision (default)
UInt = UInt' 0     -- arbitrary precision unsigned
Float = Float' 64  -- double-precision float
Byte = UInt' 8     -- unsigned byte

Short = Int' 16    -- custom alias example
Word = UInt' 32
```

#### Arbitrary Precision (width = 0)

When the width is 0, the integer has **arbitrary precision** — it will never
overflow. Small values are stored inline as 64-bit integers for performance;
on overflow, values automatically promote to heap-allocated bignums:

```milang
a = 2 ** 100   -- 1267650600228229401496703205376 (auto-promoted bignum)
b = 9223372036854775807 + 1  -- 9223372036854775808 (overflow → bignum)
c = 42 + 1     -- 43 (stays as int64, no overhead)
```

All integer arithmetic (including bare literals) automatically detects overflow
and promotes to bignums. Since `Int = Int' 0` and `UInt = UInt' 0`, arbitrary
precision is the default.

#### Fixed-Width Integers (width > 0)

-   Signed integers (`Int' n`) use two's-complement wrapping: arithmetic is
    performed modulo 2^n with results in [-2^(n-1), 2^(n-1)-1].

-   Unsigned integers (`UInt' n`) are arithmetic modulo 2^n with values in
    [0, 2^n-1]. Mixing signed and unsigned operands promotes to the wider width;
    if any operand is unsigned, the result is unsigned.

-   Floating-point types (`Float' 32`, `Float' 64`) correspond to IEEE
    single- and double-precision. Mixed-width float arithmetic promotes to the
    wider precision.

#### Promotion Rules

-   Result width is the maximum of the operand widths.
-   Mixed signed/unsigned uses the unsigned interpretation at the promoted width.
-   Arithmetic between arbitrary-precision and fixed-width uses arbitrary
    precision for the result.
-   Fixed-width clamping happens both at compile time (Haskell partial evaluator)
    and at runtime (C runtime), ensuring consistent behavior.

#### Practical Notes

-   The width argument must be a compile-time constant.
-   Use fixed widths (`Int' 8`, `Int' 32`, etc.) for FFI interop, binary
    formats, and embedded targets.
-   Use `Int`/`UInt` (or bare literals) for general-purpose code — overflow is
    handled automatically.


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

The type checker is bidirectional: it pushes `::` annotations downward and infers types bottom-up. Type errors are reported as errors. Checking is structural — records match by shape (field names and types), not by name. Any record with the right `fields` satisfies a record type.

## Additive Type Annotations (Ad-Hoc Polymorphism)

Multiple `::` annotations on the same binding declare an overloaded function. The type checker tries each annotation and succeeds if **any** of them match the actual usage:

```milang
map :: (a : b) : List : List
map :: (a : b) : Maybe : Maybe
map f xs = ...
```

This lets a single function work across different types without a trait system. The prelude uses additive annotations for functions like `map`, `fold`, `filter`, `concat`, and `flatMap` so they work on both `List` and `Maybe` values.

Additive annotations are purely additive — each `::` line adds an alternative, it never replaces a previous one. This is useful for extending prelude functions in your own code:

```milang
-- Add a new overload for map on a custom type
map :: (a : b) : MyContainer : MyContainer
```
