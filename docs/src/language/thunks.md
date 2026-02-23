# Thunks & Laziness

Milang is **eager by default** — every expression is evaluated as soon as it is
bound. The tilde operator `~` lets you opt into delayed evaluation where you
need it.

## Creating thunks with `~`

Prefixing an expression with `~` wraps it in a *thunk*: a suspended computation
that is not executed until its value is actually needed.

```milang,run
eager = 1 + 2       -- evaluated immediately
delayed = ~(1 + 2)  -- wrapped in a thunk; not yet evaluated
result = delayed     -- forced here: evaluates to 3
```

When a thunk is used in a context that needs its value (passed to an operator,
printed, pattern-matched, etc.) it is *forced* automatically — you never call a
thunk explicitly.

## `if` and auto-quote parameters

In earlier versions of milang, `if` required explicit thunks on both branches
to prevent eager evaluation:

```milang
-- Old style (still works, but no longer necessary):
result = if (x > 5) ~(x * 2) ~(x * 3)
```

The current `if` uses **auto-quote parameters** (`#param`) so the compiler
automatically quotes each branch at the call site. You can now write:

```milang,run
x = 10
result = if (x > 5) (x * 2) (x * 3)
```

Both styles work — if you pass a thunk to an auto-quote parameter, the thunk
is forced after splicing. See the [Metaprogramming](metaprogramming.md) chapter
for details on `#`-params.

## Nested conditionals

Conditionals compose naturally:

```milang,run
z = 7
result = if (z > 10) 100 (if (z > 5) 50 0)
```

Each inner `if` is only evaluated when its enclosing branch is selected.

## Lazy bindings with `:=`

The `:=` operator creates a *lazy binding* — syntactic sugar for a thunk that
caches its result after the first force:

```milang,run
x = 3
y := x + 10   -- not evaluated until y is used
z = y * 2     -- forces y here; y becomes 13, z becomes 26
```

Lazy bindings are useful for expensive computations that may never be needed, or
for establishing declaration-order dependencies without paying upfront cost.

## When to use thunks

| Situation | Mechanism |
|---|---|
| Conditional branches (`if`) | Auto-quote params handle this; `~` no longer needed |
| Short-circuit logic (`&&`, `\|\|`) | Auto-quote params handle the lazy operand |
| Deferred expensive work | Lazy binding `:=` |
| Controlling IO ordering | Thunks delay side effects until forced |

The general rule: reach for `~` whenever you need to **control when** an
expression is evaluated rather than relying on milang's default left-to-right
eager order.
