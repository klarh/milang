# Lists

Lists in milang are singly-linked cons cells, declared in the prelude as
`List = {Nil; Cons head tail}`. The bracket syntax is sugar that desugars into
this representation.

## Constructing Lists

```milang,run
nums = [1, 2, 3, 4, 5]
empty = []
consed = 10 : 20 : 30 : []
```

`[]` is `Nil`, and `[1, 2, 3]` desugars to `Cons 1 (Cons 2 (Cons 3 Nil))`.
The `:` operator (cons) is right-associative.

Use `range` to generate a sequence:

```milang,run
a = range 1 6
b = range 1 11
```

## Accessing Elements

`head`, `tail`, `last`, and `init` all return `Maybe` values — `Just x` on
success, `Nothing` on an empty list. `at` returns `Maybe` for index access.

```milang,run
xs = [10, 20, 30]
a = head xs
b = tail xs
c = last xs
d = init xs
e = at xs 1
f = head []
```

`len` returns the number of elements:

```milang,run
a = len [1, 2, 3]
b = len []
```

## Transforming

### map

Apply a function to every element:

```milang,run
doubled = map (\x -> x * 2) [1, 2, 3, 4, 5]
```

### filter

Keep elements satisfying a predicate:

```milang,run
evens = filter (\x -> x % 2 == 0) [1, 2, 3, 4, 5, 6]
```

### fold

Left-fold with an accumulator:

```milang,run
total = fold (+) 0 [1, 2, 3, 4, 5]
```

### reverse

```milang,run
backwards = reverse [1, 2, 3, 4, 5]
```

### take / drop

```milang,run
front = take 3 [1, 2, 3, 4, 5]
back = drop 3 [1, 2, 3, 4, 5]
```

### zip

Pair up elements from two lists:

```milang,run
pairs = zip [1, 2, 3] [10, 20, 30]
```

### enumerate

Produce `[index, value]` pairs:

```milang,run
indexed = enumerate ["a", "b", "c"]
```

## Combining Lists

```milang,run
joined = concat [1, 2] [3, 4]
appended = push [1, 2, 3] 4
```

`join` concatenates a list of strings with a separator:

```milang,run
csv = join ", " ["alice", "bob", "carol"]
```

## Querying

```milang,run
xs = [1, 2, 3, 4, 5]
a = sum xs
b = product xs
c = any (\x -> x > 4) xs
d = all (\x -> x > 0) xs
e = contains xs 3
f = contains xs 99
```

## Pipelines

Lists work naturally with the pipe operator for readable data processing:

```milang,run
result = range 1 11 \
  |> filter (\x -> x % 2 == 0) \
  |> map (\x -> x * x) \
  |> sum
```

## Pattern Matching on Lists

Match by exact length with `[a, b, c]`, or match `head` and `tail` with
`[first, ...rest]`:

```milang,run
xs = [10, 20, 30, 40]
result = xs ->
  [a, b, ...rest] = a + b
  [] = 0
```

Recursive functions often pattern-match to walk a list:

```milang
mySum xs = xs ->
  [x, ...rest] = x + mySum rest
  [] = 0
```
