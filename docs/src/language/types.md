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

The primitive type constructors `Int'`, `UInt'`, and `Float'` take a compile-time bit width:

```milang
add8 :: Int' 8 : Int' 8 : Int' 8
add8 a b = a + b

compact :: Float' 32 : Float' 32
compact x = x * 1.0
```

The prelude defines convenient aliases:

```milang
Int = Int' 0
UInt = UInt' 0
Float = Float' 64
Byte = UInt' 8
```

You can define your own aliases:

```milang
Short = Int' 16
Word = UInt' 32
```

### Details on Sized Numeric Types

The primitive constructors `Int'`, `UInt'`, and `Float'` take a compile-time
bit-width argument and serve as both **type constructors** (for annotations) and
**value constructors** (for creating sized values):

```milang
x = Int' 8 42     -- x is a signed 8-bit integer with value 42
y = UInt' 16 1000  -- y is an unsigned 16-bit integer
z = Int 42         -- z is an arbitrary-precision integer (Int = Int' 0)
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
and promotes to bignums. The prelude aliases `Int = Int' 0` and `UInt = UInt' 0`
make arbitrary precision the default.

#### Fixed-Width Integers (width > 0)

-   Signed integers (`Int' n`) use two's-complement semantics; arithmetic on
    signed integers is performed modulo 2^n and results are interpreted in two's
    complement when read as signed values. Overflow wraps around (modular
    arithmetic).

-   Unsigned integers (`UInt' n`) are arithmetic modulo 2^n with values in the
    range [0, 2^n-1]. Mixing signed and unsigned operands follows a conservative
    promotion model: the operands are first promoted to the wider bit-width and
    if any operand is unsigned the operation is performed in the unsigned domain
    of that width.

-   Floating-point types (`Float' 32`, `Float' 64`) correspond to standard
    IEEE-like single- and double-precision floats. Arithmetic on mixed-width
    floats promotes to the wider precision before performing the operation.

#### Promotion and Result Width

-   For fixed-width integer arithmetic, the result width is the maximum of the
    operand widths after promotion; the resulting value is wrapped/clamped to
    that width as described above.
-   For mixed signed/unsigned arithmetic the operation is performed in the
    unsigned interpretation of the promoted width.
-   Arithmetic between arbitrary-precision and fixed-width integers uses
    arbitrary precision for the result.

#### Compile-time Requirements and Partial Evaluation

-   The bit-width argument (the `n` in `Int' n`) must be a compile-time
    constant. The reducer treats sized-type aliases (for example `Int = Int' 0`)
    as syntactic sugar and reduces type aliases away.
-   Fixed-width arithmetic is clamped both at compile time (by the Haskell
    partial evaluator) and at runtime (by the C runtime), ensuring consistent
    behavior regardless of when the computation happens.

#### Practical Notes

-   Use `Int' 8`, `Int' 16`, `Int' 32`, `Int' 64` when you need explicit
    control over representation and ABI compatibility (FFI interop, binary
    formats, embedded targets).
-   Bare integer literals and the `Int`/`UInt` aliases provide arbitrary
    precision — you never need to worry about overflow in normal code.
-   The prelude exposes convenient aliases (`Int`, `UInt`, `Float`, `Byte`) for
    common use; you can define your own aliases like `Short = Int' 16`.


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
