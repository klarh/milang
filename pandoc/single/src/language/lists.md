[Header 1 ("lists", [], []) [Str "Lists"], Para [Str "Lists in milang are singly-linked cons cells, declared in the prelude as", SoftBreak, Code ("", [], []) "List = {Nil; Cons head tail}", Str ". The bracket syntax is sugar that desugars into", SoftBreak, Str "this representation."], Header 2 ("constructing-lists", ["unnumbered", "unlisted"], []) [Str "Constructing Lists"], CodeBlock ("", ["milang"], []) "nums = [1, 2, 3, 4, 5]
empty = []
consed = 10 : 20 : 30 : []
", CodeBlock ("", [""], []) "nums = [1, 2, 3, 4, 5]
empty = []
consed = [10, 20, 30]
", Para [Code ("", [], []) "[]", Str " is ", Code ("", [], []) "Nil", Str ", and ", Code ("", [], []) "[1, 2, 3]", Str " desugars to ", Code ("", [], []) "Cons 1 (Cons 2 (Cons 3 Nil))", Str ".", SoftBreak, Str "The ", Code ("", [], []) ":", Str " operator (cons) is right-associative."], Para [Str "Use ", Code ("", [], []) "range", Str " to generate a sequence:"], CodeBlock ("", ["milang"], []) "a = range 1 6
b = range 1 11
", CodeBlock ("", [""], []) "a = [1, 2, 3, 4, 5]
b = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
", Header 2 ("accessing-elements", ["unnumbered", "unlisted"], []) [Str "Accessing Elements"], Para [Code ("", [], []) "head", Str ", ", Code ("", [], []) "tail", Str ", ", Code ("", [], []) "last", Str ", and ", Code ("", [], []) "init", Str " all return ", Code ("", [], []) "Maybe", Str " values — ", Code ("", [], []) "Just x", Str " on", SoftBreak, Str "success, ", Code ("", [], []) "Nothing", Str " on an empty list. ", Code ("", [], []) "at", Str " returns ", Code ("", [], []) "Maybe", Str " for index access."], CodeBlock ("", ["milang"], []) "xs = [10, 20, 30]
a = head xs
b = tail xs
c = last xs
d = init xs
e = at xs 1
f = head []
", CodeBlock ("", [""], []) "xs = [10, 20, 30]
a = Just {val = 10}
b = Just {val = [20, 30]}
c = Just {val = 30}
d = Just {val = [10, 20]}
e = Just {val = 20}
f = Nothing {}
", Para [Code ("", [], []) "len", Str " returns the number of elements:"], CodeBlock ("", ["milang"], []) "a = len [1, 2, 3]
b = len []
", CodeBlock ("", [""], []) "a = 3
b = 0
", Header 2 ("transforming", ["unnumbered", "unlisted"], []) [Str "Transforming"], Header 3 ("map", ["unnumbered", "unlisted"], []) [Str "map"], Para [Str "Apply a function to every element:"], CodeBlock ("", ["milang"], []) "doubled = map (\\x -> x * 2) [1, 2, 3, 4, 5]
", CodeBlock ("", [""], []) "doubled = [2, 4, 6, 8, 10]
", Header 3 ("filter", ["unnumbered", "unlisted"], []) [Str "filter"], Para [Str "Keep elements satisfying a predicate:"], CodeBlock ("", ["milang"], []) "evens = filter (\\x -> x % 2 == 0) [1, 2, 3, 4, 5, 6]
", CodeBlock ("", [""], []) "evens = [2, 4, 6]
", Header 3 ("fold", ["unnumbered", "unlisted"], []) [Str "fold"], Para [Str "Left-fold with an accumulator:"], CodeBlock ("", ["milang"], []) "total = fold (+) 0 [1, 2, 3, 4, 5]
", CodeBlock ("", [""], []) "total = 15
", Header 3 ("reverse", ["unnumbered", "unlisted"], []) [Str "reverse"], CodeBlock ("", ["milang"], []) "backwards = reverse [1, 2, 3, 4, 5]
", CodeBlock ("", [""], []) "backwards = [5, 4, 3, 2, 1]
", Header 3 ("take--drop", ["unnumbered", "unlisted"], []) [Str "take / drop"], CodeBlock ("", ["milang"], []) "front = take 3 [1, 2, 3, 4, 5]
back = drop 3 [1, 2, 3, 4, 5]
", CodeBlock ("", [""], []) "front = [1, 2, 3]
back = [4, 5]
", Header 3 ("zip", ["unnumbered", "unlisted"], []) [Str "zip"], Para [Str "Pair up elements from two lists:"], CodeBlock ("", ["milang"], []) "pairs = zip [1, 2, 3] [10, 20, 30]
", CodeBlock ("", [""], []) "pairs = [[1, 10], [2, 20], [3, 30]]
", Header 3 ("enumerate", ["unnumbered", "unlisted"], []) [Str "enumerate"], Para [Str "Produce ", Code ("", [], []) "[index, value]", Str " pairs:"], CodeBlock ("", ["milang"], []) "indexed = enumerate [\"a\", \"b\", \"c\"]
", CodeBlock ("", [""], []) "indexed = [[0, a], [1, b], [2, c]]
", Header 2 ("combining-lists", ["unnumbered", "unlisted"], []) [Str "Combining Lists"], CodeBlock ("", ["milang"], []) "joined = concat [1, 2] [3, 4]
appended = push [1, 2, 3] 4
", CodeBlock ("", [""], []) "joined = [1, 2, 3, 4]
appended = [1, 2, 3, 4]
", Para [Code ("", [], []) "join", Str " concatenates a list of strings with a separator:"], CodeBlock ("", ["milang"], []) "csv = join \", \" [\"alice\", \"bob\", \"carol\"]
", CodeBlock ("", [""], []) "csv = alice, bob, carol
", Header 2 ("querying", ["unnumbered", "unlisted"], []) [Str "Querying"], CodeBlock ("", ["milang"], []) "xs = [1, 2, 3, 4, 5]
a = sum xs
b = product xs
c = any (\\x -> x > 4) xs
d = all (\\x -> x > 0) xs
e = contains xs 3
f = contains xs 99
", CodeBlock ("", [""], []) "xs = [1, 2, 3, 4, 5]
a = 15
b = 120
c = 1
d = 1
e = 1
f = 0
", Header 2 ("pipelines", ["unnumbered", "unlisted"], []) [Str "Pipelines"], Para [Str "Lists work naturally with the pipe operator for readable data processing:"], CodeBlock ("", ["milang"], []) "result = range 1 11 \\
  |> filter (\\x -> x % 2 == 0) \\
  |> map (\\x -> x * x) \\
  |> sum
", CodeBlock ("", [""], []) "result = 220
", Header 2 ("pattern-matching-on-lists", ["unnumbered", "unlisted"], []) [Str "Pattern Matching on Lists"], Para [Str "Match by exact length with ", Code ("", [], []) "[a, b, c]", Str ", or match ", Code ("", [], []) "head", Str " and ", Code ("", [], []) "tail", Str " with", SoftBreak, Code ("", [], []) "[first, ...rest]", Str ":"], CodeBlock ("", ["milang"], []) "xs = [10, 20, 30, 40]
result = xs ->
  [a, b, ...rest] = a + b
  [] = 0
", CodeBlock ("", [""], []) "xs = [10, 20, 30, 40]
result = 30
", Para [Str "Recursive functions often pattern-match to walk a list:"], CodeBlock ("", ["milang"], []) "mySum xs = xs ->
  [x, ...rest] = x + mySum rest
  [] = 0
"]