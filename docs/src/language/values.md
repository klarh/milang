# Values & Literals

This chapter covers the literal forms you can write directly in source code.

## Integers

Integer literals are written as decimal digits. Negative integers use a leading
minus sign attached to the literal. At compile time integers have arbitrary
precision; at runtime they default to `int64_t` (signed 64-bit).

The type system supports sized integers via `Int'` (signed) and `UInt'`
(unsigned) type constructors that take a bit width: `Int' 8`, `Int' 32`,
`UInt' 64`, etc. The prelude provides aliases: `Int = Int' 64`,
`UInt = UInt' 64`, `Byte = UInt' 8`.

```milang,run
small = 42
zero = 0
negative = -3
big = 2 ** 32
```

## Floats

Floating-point literals require digits on both sides of the decimal point.
They default to C `double` (64-bit). Negative floats use a leading minus sign.
Sized floats are available via `Float'`: `Float' 32` for single precision,
`Float' 64` for double precision. The prelude alias `Float = Float' 64`.

```milang,run
pi = 3.14
half = 0.5
neg = -2.718
```

## Strings

Strings are double-quoted and support the escape sequences `\n`, `\t`, `\\`,
and `\"`.

```milang,run
greeting = "hello, world"
escaped = "line one\nline two"
length = len greeting
```

### Triple-Quoted Strings

Triple-quoted strings span multiple lines with automatic margin stripping
(Swift-style). The indentation of the closing `"""` defines the margin —
everything to the left of that column is removed.

```milang,run
msg = """
  Hello, world!
    indented line
  """
```

## Booleans

There is no dedicated boolean type. Milang uses integers: `1` is true, `0` is
false. Comparison and logical operators return `1` or `0`, and `if` treats `0`
as false and any non-zero value as true.

```milang,run
yes = 1
no = 0
check = 3 > 2
```

## Lists

Lists are linked-list cons cells declared in the prelude as
`List = {Nil; Cons head tail}`. The literal syntax `[1, 2, 3]` desugars into
a chain of `Cons`/`Nil` records. The cons operator `:` is right-associative.

```milang,run
nums = [1, 2, 3]
empty = []
consed = 10 : 20 : 30 : []
```

See the [Lists](lists.md) chapter for the full prelude API.

## Records

A record is a set of named `fields` enclosed in braces and separated by `;` or
newlines.

```milang,run
point = {x = 3; y = 4}
access = point.x + point.y
```

See the [Records & ADTs](records.md) chapter for updates, destructuring, and
algebraic data types.

## Constructors

An uppercase name applied to arguments creates a tagged record. Tags are
introduced by ADT declarations or used ad-hoc.

```milang,run
Shape = {Circle radius; Rect width height}
c = Circle 5
r = Rect 3 4
```

## Functions as Values

Functions are first-class values. They can be bound to names, passed as
arguments, and returned from other functions. A function that has not received
all of its arguments displays as `<closure>`.

```milang,run
add x y = x + y
inc = add 1
result = inc 10
```

See the [Functions](functions.md) chapter for lambdas, pipes, and more.
