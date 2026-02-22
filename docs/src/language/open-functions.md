# Open Function Chaining

<!-- TODO: This page needs prose written by a technical writer.
     The structure and examples below are correct; flesh out the explanations. -->

Milang supports **open function chaining** — when you redefine a function
that uses pattern matching (the `->` arrow), your new alternatives are
automatically prepended to the existing definition. The previous definition
becomes the fallback for values that don't match your new patterns.

This is milang's answer to typeclasses: no new syntax, no special declarations.
Just define the same function again with new patterns.

## How It Works

<!-- TODO: Explain the mechanism — when a function is redefined with a Case body
     that has no catch-all wildcard, the compiler chains the definitions together.
     If the new definition HAS a catch-all, it fully replaces the old one. -->

```
truthy val = val -> Nothing = 0; Just = 1
```

Because this definition has no wildcard catch-all (`_`), the compiler
prepends `Nothing = 0; Just = 1` to the existing `truthy` definition
from the prelude, which provides the fallback behavior for all other types.

## Extensible Builtins

Three core functions are designed to be extended this way:

### truthy

<!-- TODO: Explain truthy's role as the universal boolean coercion point.
     Used by if, guards, not, &&, ||. Prelude default: 0, 0.0, "", False, Nil
     are falsy; everything else is truthy. -->

```
Maybe = {Nothing; Just val}
truthy val = val -> Nothing = 0; Just = 1

if Nothing ~"yes" ~"no"   -- "no"
if (Just 42) ~"yes" ~"no" -- "yes"
not Nothing                -- 1
```

### toString

<!-- TODO: Explain toString's role and the _toString primitive fallback.
     Prelude handles True/False/Nil; _toString handles int/float/string
     at the C level. Users extend for custom types. -->

```
Maybe = {Nothing; Just val}
toString val = val ->
  Nothing = "Nothing"
  Just = "Just(" + toString val.val + ")"

toString (Just 42)  -- "Just(42)"
toString True       -- "True"
toString 42         -- "42"
```

### eq

<!-- TODO: Explain eq as the extensible equality dispatch.
     Prelude default falls through to structural ==.
     contains uses eq, so extending eq affects list membership. -->

```
Card = {Card rank suit}
cardEq a b = a.rank == b.rank
eq a b = a -> Card = cardEq a b

eq (Card 10 "H") (Card 10 "S")  -- 1 (same rank)
contains [Card 10 "H"] (Card 10 "S")  -- 1
```

## Scope Chaining

<!-- TODO: Explain that open chaining works across scopes.
     A redefinition in a With block (inside a function body) chains with
     the outer scope's definition, not just same-scope duplicates.
     Multiple levels of chaining compose naturally. -->

```
Maybe = {Nothing; Just val}
truthy val = val -> Nothing = 0; Just = 1

main world =
  Result = {Err msg; Ok val}
  truthy val = val -> Err = 0; Ok = 1
  -- truthy now handles Maybe, Result, AND all prelude types
  0
```

## Writing Extensible Functions

<!-- TODO: Explain the pattern for making your own extensible functions.
     Key rule: omit the catch-all wildcard from extending definitions.
     If you include a catch-all, your definition fully replaces the old one. -->

```
-- Base definition (has catch-all — provides default behavior)
describe val = val -> _ = "unknown"

-- Extension (no catch-all — chains with base)
describe val = val -> Circle = "a circle"; Rect = "a rectangle"

describe (Circle 5)  -- "a circle"
describe 42          -- "unknown" (falls through to base)
```
