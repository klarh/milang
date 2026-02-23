# Scopes & Bindings

Scopes are the backbone of milang's structure. Every indented block and every
brace-delimited block creates a new scope with its own bindings.

## Basic Bindings

```milang
name = expr            -- eager binding
name := expr           -- lazy binding (thunk, evaluated at most once)
name params = expr     -- function binding
```

```milang,run
x = 42
double x = x * 2
result = double x
```

## Indentation-Sensitive Scoping

Indented lines beneath a binding form a scope. There are two modes depending
on whether an explicit result expression appears after `=`.

### Explicit Body

When a binding has an expression directly after `=`, that expression is the
scope's return value. The indented children are local definitions visible only
inside that scope:

```milang,run
compute x = result
  doubled = x * 2
  result = doubled + 1
a = compute 7
```

Here `doubled` and `result` are local to `compute`. The value of `compute 7`
is the expression after `=`, which is `result` (15).

### Implicit Record (Scope-as-Record)

When a binding has **no** expression after `=` — only indented children — the
named bindings are collected into a record and returned automatically:

```milang,run
makeVec x y =
  sum = x + y
  product = x * y
v = makeVec 3 4
```

`makeVec 3 4` returns `{sum = 7, product = 12}`. This is milang's lightweight
alternative to explicit record construction.

## Inner Scopes Shadow Outer Names

A binding in an inner scope shadows any identically-named binding from an
enclosing scope. The outer binding is unaffected:

```milang,run
x = 10
f = result
  x = 99
  result = x + 1
outer = x
inner = f
```

## Inline Scopes (With Blocks)

Braces create an inline scope on a single line. The expression before the
braces is the return value, and the bindings inside are local:

```milang,run
f x = result { doubled = x * 2; result = doubled + 1 }
a = f 7
```

## Bare Expressions (Effect Statements)

A bare expression in a scope — one not bound to a name — is evaluated for its
side effect. Its result is discarded and **not** included in any implicit
record:

```milang
main world =
  world.io.println "hello"    -- effect, result discarded
  world.io.println "world"    -- effect, result discarded
  0                           -- explicit body (exit code)
```

The first two lines run `println` for their side effects. The final `0` is the
return value of `main`.

## The `main` Function Pattern

A typical `main` combines all three concepts — local bindings, bare effect
expressions, and an explicit result:

```milang
main world =
  name = "milang"                      -- local binding
  world.io.println ("Hello, " + name)  -- bare effect
  0                                    -- return value (exit code)
```

## Binding Order

- Bindings evaluate top-to-bottom (left-to-right in brace scopes).
- Later bindings may reference earlier ones.
- The compiler tracks impure (`world`-tainted) bindings and guarantees they
  execute in declaration order via an auto-monad spine.
- Pure bindings can theoretically be reordered by the optimiser.
