# Operators

Operators in milang are ordinary functions with special syntax. Every operator
can be used in prefix form by wrapping it in parentheses, and any function can
be used infix with backtick syntax.

## Arithmetic

| Operator | Meaning |
|----------|---------|
| `+`      | Addition (also string concatenation) |
| `-`      | Subtraction |
| `*`      | Multiplication |
| `/`      | Division (integer for ints, float for floats) |
| `%`      | Modulo (integers only) |
| `**`     | Exponentiation |

```milang,run
a = 2 + 3
b = 10 - 4
c = 3 * 7
d = 10 / 3
e = 10 % 3
f = 2 ** 10
```

Float division produces a decimal result:

```milang,run
a = 7.0 / 2.0
b = 3.14 * 2.0
```

## Comparison

Comparison operators return `1` (true) or `0` (false). `==` and `/=` work
structurally on records, lists, and strings.

| Operator | Meaning |
|----------|---------|
| `==`     | Equal |
| `/=`     | Not equal |
| `<`      | Less than |
| `>`      | Greater than |
| `<=`     | Less than or equal |
| `>=`     | Greater than or equal |

```milang,run
a = 3 == 3
b = 3 /= 4
c = 5 > 2
d = [1, 2] == [1, 2]
e = "hello" == "hello"
```

## Logical

Logical operators short-circuit and return `1` or `0`. `not` is a function,
not an operator.

```milang,run
a = 1 && 1
b = 1 && 0
c = 0 || 1
d = 0 || 0
e = not 0
f = not 1
```

Short-circuit evaluation means the right-hand side is never forced when the
left side determines the result:

```milang
safe = 0 && (1 / 0)   -- 0, right side never evaluated
```

## String Concatenation

The `+` operator also concatenates strings:

```milang,run
greeting = "hello" + " " + "world"
```

## Cons

The `:` operator prepends an element to a list. It is right-associative.

```milang,run
xs = 1 : 2 : 3 : []
```

## Pipe

`x |> f` is syntactic sugar for `f x`, enabling left-to-right data flow:

```milang,run
double x = x * 2
result = 5 |> double
```

## Composition

`f >> g` composes left-to-right (`\x -> g (f x)`).
`f << g` composes right-to-left (`\x -> f (g x)`).

```milang,run
double x = x * 2
inc x = x + 1
pipeline = double >> inc
a = pipeline 5
```

## Record Merge

`a <- b` produces a new record with all `fields` from `a`, overwritten by `fields`
from `b`:

```milang,run
base = {x = 1; y = 2; z = 3}
updated = base <- {x = 10; z = 30}
```

## Operators as Functions

Wrap any operator in parentheses to use it in prefix (function) position:

```milang,run
a = (+) 3 4
b = (*) 5 6
total = fold (+) 0 [1, 2, 3, 4, 5]
```

## Functions as Infix Operators

Surround a function name with backticks to use it as an infix operator:

```milang,run
bigger = 3 `max` 7
smaller = 3 `min` 7
```

## User-Defined Operators

You can define custom operators just like functions. Precedence and
associativity are set with the parse domain `:!`. See the
[Parse Declarations](parse-decls.md) and [User Operators](user-operators.md)
chapters for details.

```milang
(<=>) a b = if (a == b) 0 (if (a > b) 1 (0 - 1))
(<=>) :! {prec = 30; assoc = Left}
```
