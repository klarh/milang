[Header 1 ("functions", [], []) [Str "Functions"], Para [Str "Functions are defined with a name, parameters, ", Code ("", [], []) "=", Str ", and a body. All functions", SoftBreak, Str "are first-class, automatically curried, and can be used anywhere a value is", SoftBreak, Str "expected."], Header 2 ("definition", ["unnumbered", "unlisted"], []) [Str "Definition"], Para [Str "A function binding lists its parameters before ", Code ("", [], []) "=", Str ". The body is a single", SoftBreak, Str "expression or an indented scope."], CodeBlock ("", ["milang"], []) "add x y = x + y
result = add 3 4
", CodeBlock ("", [""], []) "add = <closure>
result = 7
", Para [Str "When the body needs local bindings, indent them under an explicit result", SoftBreak, Str "expression:"], CodeBlock ("", ["milang"], []) "distance x1 y1 x2 y2 = result
  dx = (x2 - x1) ** 2
  dy = (y2 - y1) ** 2
  result = dx + dy
a = distance 0 0 3 4
", CodeBlock ("", [""], []) "distance = <closure>
a = 25
", Header 2 ("application", ["unnumbered", "unlisted"], []) [Str "Application"], Para [Str "Function application is juxtaposition (space-separated), and it is", SoftBreak, Str "left-associative: ", Code ("", [], []) "f a b", Str " means ", Code ("", [], []) "(f a) b", Str "."], CodeBlock ("", ["milang"], []) "add 3 4          -- 7
(add 3) 4        -- same thing
", Header 2 ("lambdas", ["unnumbered", "unlisted"], []) [Str "Lambdas"], Para [Str "Anonymous functions use ", Code ("", [], []) "\\params -> body", Str "."], CodeBlock ("", ["milang"], []) "double = \\x -> x * 2
add = \\x y -> x + y
a = double 5
b = add 3 4
", CodeBlock ("", [""], []) "double = <closure>
add = <closure>
a = 10
b = 7
", Para [Str "Lambdas are ordinary values and appear frequently as arguments to higher-order", SoftBreak, Str "functions."], Header 2 ("currying--partial-application", ["unnumbered", "unlisted"], []) [Str "Currying & Partial Application"], Para [Str "Every function is automatically curried. Supplying fewer arguments than a", SoftBreak, Str "function expects returns a new function that waits for the remaining ones."], CodeBlock ("", ["milang"], []) "add x y = x + y
add5 = add 5
result = add5 10
", CodeBlock ("", [""], []) "add = <closure>
add5 = <closure>
result = 15
", Para [Str "This makes it natural to build specialised functions on the fly:"], CodeBlock ("", ["milang"], []) "doubled = map (\\x -> x * 2) [1, 2, 3, 4]
evens = filter (\\x -> x % 2 == 0) [1, 2, 3, 4, 5, 6]
total = fold (+) 0 [1, 2, 3, 4, 5]
", CodeBlock ("", [""], []) "doubled = [2, 4, 6, 8]
evens = [2, 4, 6]
total = 15
", Header 2 ("pipes--composition", ["unnumbered", "unlisted"], []) [Str "Pipes & Composition"], Para [Str "The pipe operator ", Code ("", [], []) "|>", Str " passes a value as the ", Code ("", [], []) "last", Str " argument to a function,", SoftBreak, Str "reading left-to-right:"], CodeBlock ("", ["milang"], []) "result = [1, 2, 3, 4, 5] \\
  |> map (\\x -> x * 2) \\
  |> filter (\\x -> x > 4) \\
  |> sum
", CodeBlock ("", [""], []) "result = 24
", Para [Str "Composition operators combine functions without naming an intermediate value.", SoftBreak, Code ("", [], []) ">>", Str " composes left-to-right and ", Code ("", [], []) "<<", Str " composes right-to-left:"], CodeBlock ("", ["milang"], []) "double x = x * 2
inc x = x + 1
double_then_inc = double >> inc
inc_then_double = inc >> double
a = double_then_inc 5
b = inc_then_double 5
", CodeBlock ("", [""], []) "double = <closure>
inc = <closure>
double_then_inc = <closure>
inc_then_double = <closure>
a = 11
b = 12
", Header 2 ("recursion--tail-call-optimisation", ["unnumbered", "unlisted"], []) [Str "Recursion & Tail-Call Optimisation"], Para [Str "Functions can reference themselves by name. Milang detects self-calls (and", SoftBreak, Str "mutual calls) in ", Code ("", [], []) "tail", Str " position and compiles them with goto-based trampolining,", SoftBreak, Str "so they run in constant stack space."], CodeBlock ("", ["milang"], []) "factorial n = if (n == 0) 1 (n * factorial (n - 1))
result = factorial 10
", CodeBlock ("", [""], []) "factorial = <closure>
result = 3628800
", Para [Str "A ", Code ("", [], []) "tail", Str "-recursive accumulator style avoids building up a chain of multiplications:"], CodeBlock ("", ["milang"], []) "fac_acc acc n = if (n == 0) acc (fac_acc (acc * n) (n - 1))
result = fac_acc 1 20
", CodeBlock ("", [""], []) "fac_acc = <closure>
result = 2432902008176640000
", Header 2 ("higher-order-functions", ["unnumbered", "unlisted"], []) [Str "Higher-Order Functions"], Para [Str "A higher-order function accepts or returns another function."], CodeBlock ("", ["milang"], []) "twice f x = f (f x)
inc x = x + 1
a = twice inc 3
b = twice (\\x -> x * 2) 3
", CodeBlock ("", [""], []) "twice = <closure>
inc = <closure>
a = 5
b = 12
", Header 2 ("if-is-a-function", ["unnumbered", "unlisted"], []) [Code ("", [], []) "if", Str " Is a Function"], Para [Str "Milang has zero keywords. ", Code ("", [], []) "if", Str " is an ordinary user-defined function in the", SoftBreak, Str "prelude. It uses ", Strong [Str "auto-quote parameters"], Str " (", Code ("", [], []) "#param", Str ") so the compiler", SoftBreak, Str "automatically delays evaluation of each branch — only the chosen one runs:"], CodeBlock ("", ["milang"], []) "if (x > 0) \"positive\" \"non-positive\"
", Para [Str "No special syntax is needed at the call site. The ", Code ("", [], []) "if", Str " definition uses ", Code ("", [], []) "#t", SoftBreak, Str "and ", Code ("", [], []) "#e", Str " parameters which trigger automatic quoting; inside the body, ", Code ("", [], []) "$t", Str " and", SoftBreak, Code ("", [], []) "$e", Str " splice (evaluate) only the selected branch. See the", SoftBreak, Link ("", [], []) [Str "Metaprogramming"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/single/src/language/metaprogramming.md#metaprogramming", ""), Str " chapter for details on auto-quote params,", SoftBreak, Str "and ", Link ("", [], []) [Str "Thunks & Laziness"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/single/src/language/thunks.md#thunks--laziness", ""), Str " for the older ", Code ("", [], []) "~", Str " approach."]]