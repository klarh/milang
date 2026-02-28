[Header 1 ("user-defined-operators", [], []) [Str "User-Defined Operators"], Para [Str "In milang operators are ordinary functions whose names are made of operator", SoftBreak, Str "characters (", Code ("", [], []) "+ - * / ^ < > = ! & | @ % ? :", Str "). You define, use, and pass them", SoftBreak, Str "around exactly like any other function."], Header 2 ("defining-an-operator", ["unnumbered", "unlisted"], []) [Str "Defining an operator"], Para [Str "Wrap the operator name in parentheses and define it as a normal function:"], CodeBlock ("", ["milang"], []) "(<=>) a b = if (a == b) 0 (if (a > b) 1 (0 - 1))
cmp1 = 5 <=> 3
cmp2 = 3 <=> 3
cmp3 = 1 <=> 5
", CodeBlock ("", [""], []) "<=> = <closure>
cmp1 = 1
cmp2 = 0
cmp3 = -1
", Para [Str "The definition ", Code ("", [], []) "(<=>) a b = ...", Str " creates a two-argument function. You then use", SoftBreak, Str "it infix without parentheses: ", Code ("", [], []) "5 <=> 3", Str "."], Header 2 ("setting-precedence-and-associativity", ["unnumbered", "unlisted"], []) [Str "Setting precedence and associativity"], Para [Str "By default a user-defined operator gets a low precedence. Use a ", Strong [Str "parse", SoftBreak, Str "declaration"], Str " (", Code ("", [], []) ":!", Str ") to set the precedence level and associativity. The", SoftBreak, Str "declaration must appear before the operator's first infix use:"], CodeBlock ("", ["milang"], []) "(<+>) :! {prec = 6; assoc = Left}
(<+>) a b = {x = a.x + b.x; y = a.y + b.y}

result = {x=1;y=2} <+> {x=3;y=4}
", BulletList [[Plain [Code ("", [], []) "prec", Str " — an integer; higher binds tighter (e.g. ", Code ("", [], []) "*", Str " is 7, ", Code ("", [], []) "+", Str " is 6)."]], [Plain [Code ("", [], []) "assoc", Str " — ", Code ("", [], []) "Left", Str " or ", Code ("", [], []) "Right", Str "; controls grouping of chained uses."]]], Header 2 ("operators-as-first-class-values", ["unnumbered", "unlisted"], []) [Str "Operators as first-class values"], Para [Str "Wrapping a built-in or user-defined operator in parentheses gives you a function", SoftBreak, Str "value you can pass to higher-order functions:"], CodeBlock ("", ["milang"], []) "add = (+)
result = add 3 4
", CodeBlock ("", [""], []) "add = <closure>
result = 7
", Para [Str "This is especially useful with folds and maps:"], CodeBlock ("", ["milang"], []) "total = fold (+) 0 [1, 2, 3, 4, 5]
", Header 2 ("functions-as-infix-operators", ["unnumbered", "unlisted"], []) [Str "Functions as infix operators"], Para [Str "The backtick syntax lets you use any two-argument function in infix position:"], CodeBlock ("", ["milang"], []) "div a b = a / b
result = 10 `div` 2
", CodeBlock ("", [""], []) "div = <closure>
result = 5
", Para [Code ("", [], []) "a `f` b", Str " is equivalent to ", Code ("", [], []) "f a b", Str ". This works with any function, not just", SoftBreak, Str "operator-named ones."], Header 2 ("prefix-vs-infix", ["unnumbered", "unlisted"], []) [Str "Prefix vs. infix"], Para [Str "Every operator can be used both ways:"], CodeBlock ("", ["milang"], []) "-- Infix (the usual way)
r1 = 5 <=> 3

-- Prefix (wrap in parens)
r2 = (<=>) 5 3
", Para [Str "Both forms are interchangeable. Prefix is handy when you want to partially apply", SoftBreak, Str "an operator or pass it as an argument."]]