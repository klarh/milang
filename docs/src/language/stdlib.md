# Standard Library Reference

This page documents all functions available in the milang prelude, C builtins,
and operators. Functions marked "extensible" can be extended for user-defined
types via [open function chaining](./open-functions.md).

## Types

```milang
Bool = {True; False}
List = {Nil; Cons head tail}
Maybe = {Nothing; Just val}

-- Sized numeric type aliases (Int', UInt', Float' are primitive constructors)
Int = Int' 64       -- signed 64-bit integer
UInt = UInt' 64     -- unsigned 64-bit integer
Float = Float' 64   -- 64-bit floating-point
Byte = UInt' 8      -- unsigned 8-bit integer
```

<!-- Sized numeric type constructors

`Int'`, `UInt'`, and `Float'` are primitive type constructors that take a
compile-time bit-width argument (for example `Int' 8`, `UInt' 32`, `Float' 64`).
They provide precise control over numeric representation and arithmetic
semantics:

- `Int' n` — signed two's-complement integer of `n` bits (wraps modulo 2^n)
- `UInt' n` — unsigned integer of `n` bits (range 0..2^n-1)
- `Float' n` — floating-point with `n`-bit precision (commonly 32 or 64)

The prelude provides friendly aliases (`Int = Int' 64`, `UInt = UInt' 64`,
`Float = Float' 64`, `Byte = UInt' 8`) for convenience. Use the sized forms
when precise width or FFI compatibility is required.
-->


## Extensible Functions

These functions are designed to be extended via [open function chaining](./open-functions.md) for user-defined types.

| Function | Signature | Description |
|----------|-----------|-------------|
| `truthy` | `a : Num` | Boolean coercion. Falsy: `0`, `0.0`, `""`, `False`, `Nil`. Truthy: everything else. Used by `if`, guards, `not`, `&&`, `\|\|`. |
| `toString` | `a : Str` | String conversion. Handles `True`, `False`, `Nil` symbolically; delegates to `_toString` for primitives (int, float, string). |
| `eq` | `a : a : Num` | Equality. Default falls through to structural `==`. Used by `contains`. |

## Core Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `id` | `a : a` | Identity function. |
| `const` | `a : b : a` | Returns first argument, ignores second. |
| `flip` | `(a : b : c) : b : a : c` | Flips the first two arguments of a function. |
| `not` | `a : Num` | Logical negation via `truthy`. |

## List Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `null` | `List : Num` | Returns `1` if list is `Nil`, `0` otherwise. |
| `head` | `List : Maybe` | First element wrapped in `Maybe` (`Nothing` if empty). |
| `tail` | `List : Maybe` | Tail wrapped in `Maybe` (`Nothing` if empty). |
| `fold` | `(a : b : a) : a : List : a` | Left fold over a list. |
| `map` | `(a : b) : List : List` | Apply function to each element. |
| `filter` | `(a : Num) : List : List` | Keep elements where predicate is `truthy`. |
| `concat` | `List : List : List` | Concatenate two lists. |
| `push` | `List : a : List` | Append element to end of list. |
| `get` | `List : Num : Maybe` | Get element at index (zero-based); returns `Nothing` if out of bounds. |
| `sum` | `List : Num` | Sum of numeric list. |
| `product` | `List : Num` | Product of numeric list. |
| `any` | `(a : Num) : List : Num` | `1` if predicate is `truthy` for any element. |
| `all` | `(a : Num) : List : Num` | `1` if predicate is `truthy` for all elements. |
| `contains` | `List : a : Num` | `1` if list contains element (via `eq`). |
| `range` | `Num : Num : List` | Integer range `[start, end)`. |
| `zip` | `List : List : List` | Pair corresponding elements into 2-element lists. |
| `last` | `List : Maybe` | Last element wrapped in `Maybe` (`Nothing` if empty). |
| `init` | `List : Maybe` | All elements except the `last` wrapped in `Maybe` (`Nothing` if empty). |
| `reverse` | `List : List` | Reverse a list. |
| `take` | `Num : List : List` | First `n` elements. |
| `drop` | `Num : List : List` | Drop first `n` elements. |
| `enumerate` | `List : List` | Pair each element with its index: `[[0, a], [1, b], ...]`. |
| `join` | `Str : List : Str` | Join string list with separator. |

## Numeric Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `abs` | `Num : Num` | Absolute value. |
| `neg` | `Num : Num` | Negation (`0 - x`). |
| `min` | `Num : Num : Num` | Minimum of two numbers. |
| `max` | `Num : Num : Num` | Maximum of two numbers. |

## String Builtins

String operations provided by the C runtime:

| Function | Signature | Description |
|----------|-----------|-------------|
| `len` | `a : Num` | Length of a string or list; returns `0` for non-iterable values. |
| `strlen` | `Str : Num` | Length of a string (alias for `len`). |
| `charAt` | `Str : Num : Maybe` | Character at index; returns `Just` a single-char string when index is valid, or `Nothing` when out of range. |
| `indexOf` | `Str : Str : Num` | Index of first occurrence of substring (`-1` if not found). |
| `slice` | `Str : Num : Num : Str` | Substring from start index to end index. |
| `split` | `Str : Str : List` | Split string by delimiter. |
| `trim` | `Str : Str` | Remove leading/trailing whitespace. |
| `toUpper` | `Str : Str` | Convert to uppercase. |
| `toLower` | `Str : Str` | Convert to lowercase. |
| `replace` | `Str : Str : Str : Str` | Replace all occurrences: `replace old new str`. |

## Type Conversion Builtins

| Function | Signature | Description |
|----------|-----------|-------------|
| `toString` | `a : Str` | Convert to string (extensible — see above). |
| `toInt` | `a : Maybe` | Convert to integer; returns `Just` on success (parsing or conversion), `Nothing` on failure. |
| `toFloat` | `a : Maybe` | Convert to float; returns `Just` on success, `Nothing` on failure. |

## Record Introspection Builtins

Functions for inspecting and modifying record structure at runtime:

| Function | Signature | Description |
|----------|-----------|-------------|
| `tag` | `Record : Str` | Constructor `tag` name (e.g., `tag (Just 1)` -> `"Just"`). |
| `fields` | `Record : List` | List of field values; returns `[]` for non-record values. |
| `fieldNames` | `Record : List` | List of field names; returns `[]` for non-record values. |
| `getField` | `Record : Str : Maybe` | Dynamic field access by name; returns `Just value` if present, `Nothing` otherwise. |
| `setField` | `Record : Str : a : Record` | Return copy with field updated; on non-record values returns the original value unchanged. |

## Operators

| Operator | Signature | Description |
|----------|-----------|-------------|
| `\|>` | `a : (a : b) : b` | Pipe forward: `x \|> f` = `f x`. |
| `>>` | `(a : b) : (b : c) : a : c` | Forward composition. |
| `<<` | `(b : c) : (a : b) : a : c` | Backward composition. |
| `<-` | `Record : Record : Record` | Record merge: `base <- overlay`. |
| `&&` | `a : a : Num` | Short-circuit logical AND (via `truthy`). |
| `\|\|` | `a : a : Num` | Short-circuit logical OR (via `truthy`). |
| `:` | `a : List : List` | Cons (prepend element to list). |
| `+` `-` `*` `/` `%` `**` | `Num : Num : Num` | Arithmetic (`+` also concatenates strings). |
| `==` `/=` `<` `>` `<=` `>=` | `a : a : Num` | Comparison (structural equality for records). |

## Maybe examples

```milang,run
-- Maybe-returning stdlib usage
p1 = toInt "123"
p2 = toInt "abc"
p3 = toFloat "3.14"
r = {a = 1}

show mi = mi -> Just {val} = toString val; Nothing = "Nothing"

main world =
  world.io.println (show p1)
  world.io.println (show p2)
  world.io.println (toString p3)
  world.io.println (show (getField r "a"))
  world.io.println (show (getField r "b"))
```
