# Functions

Functions are defined with a name, parameters, `=`, and a body. All functions
are first-class, automatically curried, and can be used anywhere a value is
expected.

## Definition

A function binding lists its parameters before `=`. The body is a single
expression or an indented scope.

```milang,run
add x y = x + y
result = add 3 4
```

When the body needs local bindings, indent them under an explicit result
expression:

```milang,run
hypotenuse x y = result
  squared = x ** 2 + y ** 2
  result = squared ** 0.5
a = hypotenuse 3.0 4.0
```

## Application

Function application is juxtaposition (space-separated), and it is
left-associative: `f a b` means `(f a) b`.

```milang
add 3 4          -- 7
(add 3) 4        -- same thing
```

## Lambdas

Anonymous functions use `\params -> body`.

```milang,run
double = \x -> x * 2
add = \x y -> x + y
a = double 5
b = add 3 4
```

Lambdas are ordinary values and appear frequently as arguments to higher-order
functions.

## Currying & Partial Application

Every function is automatically curried. Supplying fewer arguments than a
function expects returns a new function that waits for the remaining ones.

```milang,run
add x y = x + y
add5 = add 5
result = add5 10
```

This makes it natural to build specialised functions on the fly:

```milang,run
doubled = map (\x -> x * 2) [1, 2, 3, 4]
evens = filter (\x -> x % 2 == 0) [1, 2, 3, 4, 5, 6]
total = fold (+) 0 [1, 2, 3, 4, 5]
```

## Pipes & Composition

The pipe operator `|>` passes a value as the `last` argument to a function,
reading left-to-right:

```milang,run
result = [1, 2, 3, 4, 5] \
  |> map (\x -> x * 2) \
  |> filter (\x -> x > 4) \
  |> sum
```

Composition operators combine functions without naming an intermediate value.
`>>` composes left-to-right and `<<` composes right-to-left:

```milang,run
double x = x * 2
inc x = x + 1
double_then_inc = double >> inc
inc_then_double = inc >> double
a = double_then_inc 5
b = inc_then_double 5
```

## Recursion & Tail-Call Optimisation

Functions can reference themselves by name. Milang detects self-calls (and
mutual calls) in `tail` position and compiles them with goto-based trampolining,
so they run in constant stack space.

```milang,run
factorial n = if (n == 0) 1 (n * factorial (n - 1))
result = factorial 10
```

A `tail`-recursive accumulator style avoids building up a chain of multiplications:

```milang,run
fac_acc acc n = if (n == 0) acc (fac_acc (acc * n) (n - 1))
result = fac_acc 1 20
```

## Higher-Order Functions

A higher-order function accepts or returns another function.

```milang,run
twice f x = f (f x)
inc x = x + 1
a = twice inc 3
b = twice (\x -> x * 2) 3
```

## `if` Is a Function

Milang has zero keywords. `if` is an ordinary user-defined function in the
prelude. It uses **auto-quote parameters** (`#param`) so the compiler
automatically delays evaluation of each branch — only the chosen one runs:

```milang
if (x > 0) "positive" "non-positive"
```

No special syntax is needed at the call site. The `if` definition uses `#t`
and `#e` parameters which trigger automatic quoting; inside the body, `$t` and
`$e` splice (evaluate) only the selected branch. See the
[Metaprogramming](metaprogramming.md) chapter for details on auto-quote params,
and [Thunks & Laziness](thunks.md) for the older `~` approach.
