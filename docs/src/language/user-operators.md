# User-Defined Operators

In milang operators are ordinary functions whose names are made of operator
characters (`+ - * / ^ < > = ! & | @ % ? :`). You define, use, and pass them
around exactly like any other function.

## Defining an operator

Wrap the operator name in parentheses and define it as a normal function:

```milang,run
(<=>) a b = if (a == b) ~0 ~(if (a > b) ~1 ~(0 - 1))
cmp1 = 5 <=> 3
cmp2 = 3 <=> 3
cmp3 = 1 <=> 5
```

The definition `(<=>) a b = ...` creates a two-argument function. You then use
it infix without parentheses: `5 <=> 3`.

## Setting precedence and associativity

By default a user-defined operator gets a low precedence. Use a **parse
declaration** (`:!`) to set the precedence level and associativity. The
declaration must appear before the operator's first infix use:

```milang
(<+>) :! {prec = 6; assoc = Left}
(<+>) a b = {x = a.x + b.x; y = a.y + b.y}

result = {x=1;y=2} <+> {x=3;y=4}
```

- `prec` — an integer; higher binds tighter (e.g. `*` is 7, `+` is 6).
- `assoc` — `Left` or `Right`; controls grouping of chained uses.

## Operators as first-class values

Wrapping a built-in or user-defined operator in parentheses gives you a function
value you can pass to higher-order functions:

```milang,run
add = (+)
result = add 3 4
```

This is especially useful with folds and maps:

```milang
total = fold (+) 0 [1, 2, 3, 4, 5]
```

## Functions as infix operators

The backtick syntax lets you use any two-argument function in infix position:

```milang,run
div a b = a / b
result = 10 `div` 2
```

`` a `f` b `` is equivalent to `f a b`. This works with any function, not just
operator-named ones.

## Prefix vs. infix

Every operator can be used both ways:

```milang
-- Infix (the usual way)
r1 = 5 <=> 3

-- Prefix (wrap in parens)
r2 = (<=>) 5 3
```

Both forms are interchangeable. Prefix is handy when you want to partially apply
an operator or pass it as an argument.
