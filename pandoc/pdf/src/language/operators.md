[Header 1 ("operators", [], []) [Str "Operators"], Para [Str "Operators in milang are ordinary functions with special syntax. Every operator", SoftBreak, Str "can be used in prefix form by wrapping it in parentheses, and any function can", SoftBreak, Str "be used infix with backtick syntax."], Header 2 ("arithmetic", ["unnumbered", "unlisted"], []) [Str "Arithmetic"], Table ("", [], []) (Caption Nothing []) [(AlignDefault, ColWidthDefault), (AlignDefault, ColWidthDefault)] (TableHead ("", [], []) [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Operator"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Meaning"]]]]) [(TableBody ("", [], []) (RowHeadColumns 0) [] [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "+"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Addition (also string concatenation)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "-"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Subtraction"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "*"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Multiplication"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "/"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Division (integer for ints, float for floats)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "%"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Modulo (integers only)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "**"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Exponentiation (integer exponent)"]]]])] (TableFoot ("", [], []) []), CodeBlock ("", ["milang"], []) "a = 2 + 3
b = 10 - 4
c = 3 * 7
d = 10 / 3
e = 10 % 3
f = 2 ** 10
", CodeBlock ("", [""], []) "a = 5
b = 6
c = 21
d = 3
e = 1
f = 1024
", Para [Str "Float division produces a decimal result:"], CodeBlock ("", ["milang"], []) "a = 7.0 / 2.0
b = 3.14 * 2.0
", CodeBlock ("", [""], []) "a = 3.5
b = 6.28
", Header 2 ("comparison", ["unnumbered", "unlisted"], []) [Str "Comparison"], Para [Str "Comparison operators return ", Code ("", [], []) "1", Str " (true) or ", Code ("", [], []) "0", Str " (false). ", Code ("", [], []) "==", Str " and ", Code ("", [], []) "/=", Str " work", SoftBreak, Str "structurally on records, lists, and strings."], Table ("", [], []) (Caption Nothing []) [(AlignDefault, ColWidthDefault), (AlignDefault, ColWidthDefault)] (TableHead ("", [], []) [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Operator"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Meaning"]]]]) [(TableBody ("", [], []) (RowHeadColumns 0) [] [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "=="]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Equal"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "/="]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Not equal"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "<"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Less than"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) ">"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Greater than"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "<="]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Less than or equal"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) ">="]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Greater than or equal"]]]])] (TableFoot ("", [], []) []), CodeBlock ("", ["milang"], []) "a = 3 == 3
b = 3 /= 4
c = 5 > 2
d = [1, 2] == [1, 2]
e = \"hello\" == \"hello\"
", CodeBlock ("", [""], []) "a = 1
b = 1
c = 1
d = 1
e = 1
", Header 2 ("logical", ["unnumbered", "unlisted"], []) [Str "Logical"], Para [Str "Logical operators short-circuit and return ", Code ("", [], []) "1", Str " or ", Code ("", [], []) "0", Str ". ", Code ("", [], []) "not", Str " is a function,", SoftBreak, Str "not an operator."], CodeBlock ("", ["milang"], []) "a = 1 && 1
b = 1 && 0
c = 0 || 1
d = 0 || 0
e = not 0
f = not 1
", CodeBlock ("", [""], []) "a = 1
b = 0
c = 1
d = 0
e = 1
f = 0
", Para [Str "Short-circuit evaluation means the right-hand side is never forced when the", SoftBreak, Str "left side determines the result:"], CodeBlock ("", ["milang"], []) "safe = 0 && (1 / 0)   -- 0, right side never evaluated
", Header 2 ("string-concatenation", ["unnumbered", "unlisted"], []) [Str "String Concatenation"], Para [Str "The ", Code ("", [], []) "+", Str " operator also concatenates strings:"], CodeBlock ("", ["milang"], []) "greeting = \"hello\" + \" \" + \"world\"
", CodeBlock ("", [""], []) "greeting = hello world
", Header 2 ("cons", ["unnumbered", "unlisted"], []) [Str "Cons"], Para [Str "The ", Code ("", [], []) ":", Str " operator prepends an element to a list. It is right-associative."], CodeBlock ("", ["milang"], []) "xs = 1 : 2 : 3 : []
", CodeBlock ("", [""], []) "xs = [1, 2, 3]
", Header 2 ("pipe", ["unnumbered", "unlisted"], []) [Str "Pipe"], Para [Code ("", [], []) "x |> f", Str " is syntactic sugar for ", Code ("", [], []) "f x", Str ", enabling left-to-right data flow:"], CodeBlock ("", ["milang"], []) "double x = x * 2
result = 5 |> double
", CodeBlock ("", [""], []) "double = <closure>
result = 10
", Header 2 ("composition", ["unnumbered", "unlisted"], []) [Str "Composition"], Para [Code ("", [], []) "f >> g", Str " composes left-to-right (", Code ("", [], []) "\\x -> g (f x)", Str ").", SoftBreak, Code ("", [], []) "f << g", Str " composes right-to-left (", Code ("", [], []) "\\x -> f (g x)", Str ")."], CodeBlock ("", ["milang"], []) "double x = x * 2
inc x = x + 1
pipeline = double >> inc
a = pipeline 5
", CodeBlock ("", [""], []) "double = <closure>
inc = <closure>
pipeline = <closure>
a = 11
", Header 2 ("record-merge", ["unnumbered", "unlisted"], []) [Str "Record Merge"], Para [Code ("", [], []) "a <- b", Str " produces a new record with all ", Code ("", [], []) "fields", Str " from ", Code ("", [], []) "a", Str ", overwritten by ", Code ("", [], []) "fields", SoftBreak, Str "from ", Code ("", [], []) "b", Str ":"], CodeBlock ("", ["milang"], []) "base = {x = 1; y = 2; z = 3}
updated = base <- {x = 10; z = 30}
", CodeBlock ("", [""], []) "base =  {x = 1, y = 2, z = 3}
updated =  {x = 10, y = 2, z = 30}
", Header 2 ("operators-as-functions", ["unnumbered", "unlisted"], []) [Str "Operators as Functions"], Para [Str "Wrap any operator in parentheses to use it in prefix (function) position:"], CodeBlock ("", ["milang"], []) "a = (+) 3 4
b = (*) 5 6
total = fold (+) 0 [1, 2, 3, 4, 5]
", CodeBlock ("", [""], []) "a = 7
b = 30
total = 15
", Header 2 ("functions-as-infix-operators", ["unnumbered", "unlisted"], []) [Str "Functions as Infix Operators"], Para [Str "Surround a function name with backticks to use it as an infix operator:"], CodeBlock ("", ["milang"], []) "bigger = 3 `max` 7
smaller = 3 `min` 7
", CodeBlock ("", [""], []) "bigger = 7
smaller = 3
", Header 2 ("user-defined-operators", ["unnumbered", "unlisted"], []) [Str "User-Defined Operators"], Para [Str "You can define custom operators just like functions. Precedence and", SoftBreak, Str "associativity are set with the parse domain ", Code ("", [], []) ":!", Str ". See the", SoftBreak, Link ("", [], []) [Str "Parse Declarations"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/pdf/src/language/parse-decls.md#parse-declarations-", ""), Str " and ", Link ("", [], []) [Str "User Operators"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/pdf/src/language/user-operators.md#user-defined-operators", ""), SoftBreak, Str "chapters for details."], CodeBlock ("", ["milang"], []) "(<=>) a b = if (a == b) 0 (if (a > b) 1 (0 - 1))
(<=>) :! {prec = 30; assoc = Left}
"]