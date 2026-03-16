# Common Pitfalls

A guide to the most frequent mistakes and surprises in milang,
especially for developers coming from Haskell, Python, or other
mainstream languages.

## Record Literals Must Be Parenthesized as Arguments

A bare `{...}` after an expression is parsed as a **record update** (`<-`),
not a function argument.

```milang
-- WRONG: parsed as (if cond) <- {x = 1} — a record update on if's result
f cond = if cond {x = 1} {x = 2}

-- RIGHT: parenthesize the record literal
f cond = if cond ({x = 1}) ({x = 2})

-- RIGHT: bind first, then pass
f cond = if cond a b
  a = {x = 1}
  b = {x = 2}
```

When a record literal follows a function call without parentheses, the
parser treats it as a record update expression instead of a function
argument.

## Blocks Return Records, Not the Last Expression

Unlike Python or Rust, a block of bindings returns the **record of all
named bindings**, not the last expression.

```milang
-- Returns {doubled = 14, tripled = 21} — NOT tripled
compute x =
  doubled = x * 2
  tripled = x * 3

-- To return a specific value, put the body expression FIRST:
compute x = tripled
  doubled = x * 2
  tripled = x * 3
```

The with-binding pattern — body expression on the same line as `=`,
local bindings indented below — is the standard way to write functions
that return a specific value.

## Line Continuation with `\`

Long expressions can be split across lines using `\` at the end of a line:

```milang
result = very_long_function_name arg1 \
  arg2 arg3
```

The `\` joins the next line to the current one, ignoring the newline
and indentation.

## `if` Is a Function, Not a Keyword

There are no keywords in milang.  `if` is a prelude function with
auto-quoted parameters (`#`):

```milang
if cond #t #e = (truthy cond) -> 0 = $e; _ = $t
```

This means:
- Both branches must be expressions (no statements or blocks)
- Both branches are provided as arguments: `if condition trueVal falseVal`
- There is no `then`/`else` keyword

```milang
-- Milang:
max a b = if (a > b) a b

-- Haskell equivalent:
-- max a b = if a > b then a else b

-- Python equivalent:
-- def max(a, b): return a if a > b else b
```

For multi-line if expressions, keep the entire `if` on one line or use
with-bindings:

```milang
-- WRONG: else branch at same indent as if → parsed as separate statement
f x =
  if (x > 0) "positive"
  "non-positive"

-- RIGHT: one line
f x = if (x > 0) "positive" "non-positive"

-- RIGHT: with-bindings
f x = result
  result = if (x > 0) "positive" "non-positive"
```

## Truthiness: Integers, Not Booleans

Milang uses integers for truth values.  `1` is true, `0` is false.
`True` and `False` are constructors (tagged records), but comparisons
and `if` work with integers:

```milang
-- Comparisons return 0 or 1
3 > 2          -- 1
3 == 3         -- 1
"a" == "b"     -- 0

-- Truthy: anything non-zero, non-empty, non-Nothing, non-Nil
-- Falsy: 0, "", False, Nil, Nothing
if 42 "yes" "no"       -- "yes"
if "" "yes" "no"       -- "no"
if Nothing "yes" "no"  -- "no"

-- Convert any value to 0/1 with `truthy`:
truthy 42        -- 1
truthy ""        -- 0
truthy [1, 2]    -- 1
truthy Nothing   -- 0
```

## Not-Equal Is `/=`, Not `!=`

```milang
-- WRONG: != is not defined
x != y

-- RIGHT: use /= (same as Haskell)
x /= y

-- Also works: negate ==
not (x == y)
```

## String Concatenation Uses `+`

Unlike Haskell (`++`) or Python (implicit), milang uses `+` for both
numeric addition and string concatenation:

```milang
"hello" + " " + "world"   -- "hello world"
3 + 4                       -- 7
```

## No Mutable Variables or Loops

There are no `for`, `while`, or assignment statements.  Use recursion
or higher-order prelude functions:

```milang
-- Python:  sum = 0; for x in range(10): sum += x
-- Milang:
total = fold (\acc x -> acc + x) 0 (range 0 10)

-- Python:  result = [x*2 for x in items]
-- Milang:
result = map (\x -> x * 2) items

-- Python:  evens = [x for x in items if x % 2 == 0]
-- Milang:
evens = filter (\x -> x % 2 == 0) items
```

For iterative algorithms, use tail-recursive functions with the
with-binding pattern:

```milang
-- Iterate until condition met
loop n = if (n >= 1000) n (loop next)
  next = n + 1
```

## Lists Are Linked Lists

Milang lists are Cons/Nil linked lists, not arrays:

```milang
[1, 2, 3]    -- desugars to Cons 1 (Cons 2 (Cons 3 Nil))
```

This means:
- `head`, `tail`, cons (`:`) are O(1) — access/modify the front
- `len` is O(n) — must traverse the entire list to count elements
- `at i lst` is O(i) — must walk i links to reach element i
- `last` is O(n) — must walk to the final element
- `push` (append to end) is O(n) — must copy the entire spine
- Prepend with `:` for O(1): `x : xs`

```milang
-- SLOW: O(n²) — appends to end each iteration
build lst = fold (\acc x -> push acc x) [] items

-- FAST: O(n) — prepend then reverse
build lst = reverse (fold (\acc x -> x : acc) [] items)
```

## Imports Return Records

`import` returns a record of all top-level bindings from the imported
file.  There is no automatic namespace injection:

```milang
-- Python:  from math import sqrt
-- Haskell: import Math (sqrt)
-- Milang:
math = import "math.mi"
result = math.sqrt 16

-- To use names directly, destructure with {...}:
{sqrt; sin; cos} = import "math.mi"
result = sqrt 16
```

## Pattern Matching Uses `->`, Not `case ... of`

```milang
-- Haskell:
-- case x of
--   0 -> "zero"
--   n -> "other"

-- Milang:
x -> 0 = "zero"; n = "other"

-- Or indented:
x ->
  0 = "zero"
  n = "other"
```

Patterns use `=` to separate pattern from body (not `->` as in Haskell).

## `Maybe` Everywhere

Functions that might fail return `Maybe` (`Just val` or `Nothing`),
not exceptions or sentinel values:

```milang
head []           -- Nothing
head [1, 2, 3]    -- Just 1
at [10, 20] 5     -- Nothing (out of bounds)
toInt "abc"       -- Nothing
toInt "42"        -- Just 42

-- Unwrap with pattern matching:
result = toInt input ->
  Just n = n * 2
  Nothing = 0

-- Or use default:
n = default 0 (toInt input)
```

## Semicolons Are Field Separators, Not Statement Terminators

In records and inline scopes, `;` separates fields.  It is not a
statement terminator:

```milang
point = {x = 1; y = 2}           -- record with two fields
f x = x -> 0 = "a"; _ = "b"     -- pattern match with two alternatives
{a = 1; b = 2; c = 3}            -- three-field record
```

## No Parentheses Around Function Arguments

Unlike Python/C, function application is by juxtaposition.  Only use
parentheses to group sub-expressions:

```milang
-- WRONG (Python habit): extra parens are harmless but unnecessary
f(x)              -- works but looks odd
f(x, y)           -- WRONG: this is f applied to a tuple-like record

-- RIGHT: Haskell-style juxtaposition
f x               -- apply f to x
f x y             -- apply f to x, then apply result to y
f (x + 1) y       -- parens only to group the sub-expression x + 1
```

## Pipe Operator for Readability

Use `|>` to avoid deeply nested function calls:

```milang
-- Nested (hard to read):
toString (sum (map (\x -> x * 2) (filter (\x -> x > 0) lst)))

-- Piped (reads left to right):
lst
  |> filter (\x -> x > 0)
  |> map (\x -> x * 2)
  |> sum
  |> toString
```

## Lazy Bindings Need `:=`

A regular binding (`=`) evaluates eagerly.  Use `:=` for lazy evaluation
(thunked — computed on first access):

```milang
-- Evaluated immediately, even if never used
expensive = computeHeavyThing x

-- Evaluated only when first accessed
expensive := computeHeavyThing x
```

Prefix `~` creates an inline thunk:

```milang
pair = {now = compute x; later = ~(compute y)}
```
