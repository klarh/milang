# Standard Library Reference

<!-- TODO: This page needs prose introductions for each section.
     The signatures and descriptions below are accurate; add explanatory text,
     usage examples, and edge-case notes. -->

## Types

```
Bool = {True; False}
List = {Nil; Cons head tail}
```

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
| `head` | `List : a` | First element of a list. |
| `tail` | `List : List` | All elements after the first. |
| `fold` | `(a : b : a) : a : List : a` | Left fold over a list. |
| `map` | `(a : b) : List : List` | Apply function to each element. |
| `filter` | `(a : Num) : List : List` | Keep elements where predicate is truthy. |
| `concat` | `List : List : List` | Concatenate two lists. |
| `push` | `List : a : List` | Append element to end of list. |
| `get` | `List : Num : a` | Get element at index (zero-based). |
| `sum` | `List : Num` | Sum of numeric list. |
| `product` | `List : Num` | Product of numeric list. |
| `any` | `(a : Num) : List : Num` | `1` if predicate is truthy for any element. |
| `all` | `(a : Num) : List : Num` | `1` if predicate is truthy for all elements. |
| `contains` | `List : a : Num` | `1` if list contains element (via `eq`). |
| `range` | `Num : Num : List` | Integer range `[start, end)`. |
| `zip` | `List : List : List` | Pair corresponding elements into 2-element lists. |
| `last` | `List : a` | Last element of a list. |
| `init` | `List : List` | All elements except the last. |
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

<!-- TODO: Add brief descriptions and examples for each. -->

| Function | Signature | Description |
|----------|-----------|-------------|
| `len` | `a : Num` | Length of a string or list. |
| `strlen` | `Str : Num` | Length of a string (alias for `len`). |
| `charAt` | `Str : Num : Str` | Character at index (returns single-char string). |
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
| `toInt` | `a : Num` | Convert to integer (truncates floats, parses strings). |
| `toFloat` | `a : Float` | Convert to float (promotes ints, parses strings). |

## Record Introspection Builtins

<!-- TODO: Add examples showing typical usage patterns. -->

| Function | Signature | Description |
|----------|-----------|-------------|
| `tag` | `Record : Str` | Constructor tag name (e.g., `tag (Just 1)` → `"Just"`). |
| `fields` | `Record : List` | List of field values. |
| `fieldNames` | `Record : List` | List of field name strings. |
| `getField` | `Record : Str : a` | Get field by name. |
| `setField` | `Record : Str : a : Record` | Return copy with field updated. |

## Operators

| Operator | Signature | Description |
|----------|-----------|-------------|
| `\|>` | `a : (a : b) : b` | Pipe forward: `x \|> f` = `f x`. |
| `>>` | `(a : b) : (b : c) : a : c` | Forward composition. |
| `<<` | `(b : c) : (a : b) : a : c` | Backward composition. |
| `<-` | `Record : Record : Record` | Record merge: `base <- overlay`. |
| `&&` | `a : a : Num` | Short-circuit logical AND (via truthy). |
| `\|\|` | `a : a : Num` | Short-circuit logical OR (via truthy). |
| `:` | `a : List : List` | Cons (prepend element to list). |
| `+` `-` `*` `/` `%` `**` | `Num : Num : Num` | Arithmetic (`+` also concatenates strings). |
| `==` `/=` `<` `>` `<=` `>=` | `a : a : Num` | Comparison (structural equality for records). |
