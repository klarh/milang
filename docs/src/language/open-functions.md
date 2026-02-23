# Open Function Chaining

Milang supports **open function chaining** — when you redefine a function
that uses pattern matching (the `->` arrow), your new alternatives are
automatically prepended to the existing definition. The previous definition
becomes the fallback for values that don't match your new patterns.

This is milang's answer to typeclasses: no new syntax, no special declarations.
`Just` define the same function again with new patterns.

## How It Works

When a binding is redefined in the same scope and the new definition uses
pattern matching (`->`) **without a catch-all wildcard** (`_`), the compiler
chains the two definitions together. The new alternatives are tried first;
if none match, the old definition handles the value.

If the new definition **includes** a catch-all wildcard, it fully replaces
the old definition — the catch-all means "I handle everything."

```milang
-- Base: has catch-all
describe val = val -> _ = "unknown"

-- Extension: no catch-all — chains with base
describe val = val -> Circle = "a circle"; Rect = "a rectangle"
```

Now `describe (Circle 5)` returns `"a circle"` and `describe 42` falls
through to the base, returning `"unknown"`.

## Extensible Builtins

Three core prelude functions are designed to be extended this way:

### `truthy`

`truthy` is the universal boolean coercion point. It is called internally
by `if`, guards, `not`, `&&`, and `||`. The prelude default treats `0`,
`0.0`, `""`, `False`, `Nil`, and `Nothing` as falsy (returns `0`);
everything else is `truthy` (returns `1`).

Extend `truthy` to teach the language how your types behave in boolean
contexts:

```milang,run
Result = {Err msg; Ok val}
truthy val = val -> Err = 0; Ok = 1

main world =
  world.io.println (toString (if (Ok 42) ~"yes" ~"no"))
  world.io.println (toString (if (Err "oops") ~"yes" ~"no"))
  world.io.println (toString (not (Err "fail")))
```

### `toString`

`toString` converts values to their string representation. The prelude
handles `True`, `False`, `Nil`, `Nothing`, and `Just` symbolically, then
falls through to `_toString` (the C-level primitive) for ints, floats, and
strings. Extend it for your own types:

```milang,run
Pair = {Pair fst snd}
toString val = val -> Pair = "(" + toString val.fst + ", " + toString val.snd + ")"

main world =
  world.io.println (toString (Pair 1 2))
  world.io.println (toString (Pair "hello" True))
```

### `eq`

`eq` is the extensible equality function. The prelude default falls through
to structural `==`. The `contains` function uses `eq`, so extending `eq`
automatically affects list membership checks:

```milang,run
Card = {Card rank suit}
eq a b = a -> Card = a.rank == b.rank

main world =
  world.io.println (toString (eq (Card 10 "H") (Card 10 "S")))
  world.io.println (toString (contains [Card 10 "H", Card 5 "D"] (Card 10 "S")))
```

## Scope Chaining

Open chaining works across scopes. A redefinition inside a function body
(a `With` block) chains with the outer scope's definition, not just
same-scope duplicates. Multiple levels of chaining compose naturally:

```milang,run
Result = {Err msg; Ok val}
truthy val = val -> Err = 0; Ok = 1

main world =
  Severity = {Low; High}
  truthy val = val -> Low = 0; High = 1
  -- truthy now handles Result, Severity, AND all prelude types
  world.io.println (toString (truthy (Ok 1)))
  world.io.println (toString (truthy (Err "x")))
  world.io.println (toString (truthy High))
  world.io.println (toString (truthy Low))
  world.io.println (toString (truthy Nothing))
```

## Writing Extensible Functions

To make your own functions extensible, follow this pattern:

1. **Define a base** with a catch-all wildcard — this provides default behavior.
2. **Extend without a catch-all** — your new alternatives are prepended; the base stays as fallback.

```milang,run
-- Base definition (has catch-all)
describe val = val -> _ = "something"

-- Extension (no catch-all — chains)
Shape = {Circle radius; Rect width height}
describe val = val -> Circle = "a circle"; Rect = "a rectangle"

main world =
  world.io.println (describe (Circle 5))
  world.io.println (describe (Rect 3 4))
  world.io.println (describe 42)
```

If you include a catch-all in an extension, it fully replaces the base —
use this when you genuinely want to override all behavior.
