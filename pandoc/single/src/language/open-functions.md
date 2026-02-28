[Header 1 ("open-function-chaining", [], []) [Str "Open Function Chaining"], Para [Str "Milang supports ", Strong [Str "open function chaining"], Str " — when you redefine a function", SoftBreak, Str "that uses pattern matching (the ", Code ("", [], []) "->", Str " arrow), your new alternatives are", SoftBreak, Str "automatically prepended to the existing definition. The previous definition", SoftBreak, Str "becomes the fallback for values that don't match your new patterns."], Para [Str "This is milang's answer to typeclasses: no new syntax, no special declarations.", SoftBreak, Code ("", [], []) "Just", Str " define the same function again with new patterns."], Header 2 ("how-it-works", ["unnumbered", "unlisted"], []) [Str "How It Works"], Para [Str "When a binding is redefined in the same scope and the new definition uses", SoftBreak, Str "pattern matching (", Code ("", [], []) "->", Str ") ", Strong [Str "without a catch-all wildcard"], Str " (", Code ("", [], []) "_", Str "), the compiler", SoftBreak, Str "chains the two definitions together. The new alternatives are tried first;", SoftBreak, Str "if none match, the old definition handles the value."], Para [Str "If the new definition ", Strong [Str "includes"], Str " a catch-all wildcard, it fully replaces", SoftBreak, Str "the old definition — the catch-all means \"I handle everything.\""], CodeBlock ("", ["milang"], []) "-- Base: has catch-all
describe val = val -> _ = \"unknown\"

-- Extension: no catch-all — chains with base
describe val = val -> Circle = \"a circle\"; Rect = \"a rectangle\"
", Para [Str "Now ", Code ("", [], []) "describe (Circle 5)", Str " returns ", Code ("", [], []) "\"a circle\"", Str " and ", Code ("", [], []) "describe 42", Str " falls", SoftBreak, Str "through to the base, returning ", Code ("", [], []) "\"unknown\"", Str "."], Header 2 ("extensible-builtins", ["unnumbered", "unlisted"], []) [Str "Extensible Builtins"], Para [Str "Three core prelude functions are designed to be extended this way:"], Header 3 ("truthy", ["unnumbered", "unlisted"], []) [Code ("", [], []) "truthy"], Para [Code ("", [], []) "truthy", Str " is the universal boolean coercion point. It is called internally", SoftBreak, Str "by ", Code ("", [], []) "if", Str ", guards, ", Code ("", [], []) "not", Str ", ", Code ("", [], []) "&&", Str ", and ", Code ("", [], []) "||", Str ". The prelude default treats ", Code ("", [], []) "0", Str ",", SoftBreak, Code ("", [], []) "0.0", Str ", ", Code ("", [], []) "\"\"", Str ", ", Code ("", [], []) "False", Str ", ", Code ("", [], []) "Nil", Str ", and ", Code ("", [], []) "Nothing", Str " as falsy (returns ", Code ("", [], []) "0", Str ");", SoftBreak, Str "everything else is ", Code ("", [], []) "truthy", Str " (returns ", Code ("", [], []) "1", Str ")."], Para [Str "Extend ", Code ("", [], []) "truthy", Str " to teach the language how your types behave in boolean", SoftBreak, Str "contexts:"], CodeBlock ("", ["milang"], []) "Result = {Err msg; Ok val}
truthy val = val -> Err = 0; Ok = 1

main world =
  world.io.println (toString (if (Ok 42) \"yes\" \"no\"))
  world.io.println (toString (if (Err \"oops\") \"yes\" \"no\"))
  world.io.println (toString (not (Err \"fail\")))
", CodeBlock ("", [""], []) "yes
no
1
", Header 3 ("tostring", ["unnumbered", "unlisted"], []) [Code ("", [], []) "toString"], Para [Code ("", [], []) "toString", Str " converts values to their string representation. The prelude", SoftBreak, Str "handles ", Code ("", [], []) "True", Str ", ", Code ("", [], []) "False", Str ", ", Code ("", [], []) "Nil", Str ", ", Code ("", [], []) "Nothing", Str ", and ", Code ("", [], []) "Just", Str " symbolically, then", SoftBreak, Str "falls through to ", Code ("", [], []) "_toString", Str " (the C-level primitive) for ints, floats, and", SoftBreak, Str "strings. Extend it for your own types:"], CodeBlock ("", ["milang"], []) "Pair = {Pair fst snd}
toString val = val -> Pair = \"(\" + toString val.fst + \", \" + toString val.snd + \")\"

main world =
  world.io.println (toString (Pair 1 2))
  world.io.println (toString (Pair \"hello\" True))
", CodeBlock ("", [""], []) "(1, 2)
(hello, True)
", Header 3 ("eq", ["unnumbered", "unlisted"], []) [Code ("", [], []) "eq"], Para [Code ("", [], []) "eq", Str " is the extensible equality function. The prelude default falls through", SoftBreak, Str "to structural ", Code ("", [], []) "==", Str ". The ", Code ("", [], []) "contains", Str " function uses ", Code ("", [], []) "eq", Str ", so extending ", Code ("", [], []) "eq", SoftBreak, Str "automatically affects list membership checks:"], CodeBlock ("", ["milang"], []) "Card = {Card rank suit}
eq a b = a -> Card = a.rank == b.rank

main world =
  world.io.println (toString (eq (Card 10 \"H\") (Card 10 \"S\")))
  world.io.println (toString (contains [Card 10 \"H\", Card 5 \"D\"] (Card 10 \"S\")))
", CodeBlock ("", [""], []) "1
0
", Header 2 ("scope-chaining", ["unnumbered", "unlisted"], []) [Str "Scope Chaining"], Para [Str "Open chaining works across scopes. A redefinition inside a function body", SoftBreak, Str "(a ", Code ("", [], []) "With", Str " block) chains with the outer scope's definition, not just", SoftBreak, Str "same-scope duplicates. Multiple levels of chaining compose naturally:"], CodeBlock ("", ["milang"], []) "Result = {Err msg; Ok val}
truthy val = val -> Err = 0; Ok = 1

main world =
  Severity = {Low; High}
  truthy val = val -> Low = 0; High = 1
  -- truthy now handles Result, Severity, AND all prelude types
  world.io.println (toString (truthy (Ok 1)))
  world.io.println (toString (truthy (Err \"x\")))
  world.io.println (toString (truthy High))
  world.io.println (toString (truthy Low))
  world.io.println (toString (truthy Nothing))
", CodeBlock ("", [""], []) "1
0
1
0
0
", Header 2 ("writing-extensible-functions", ["unnumbered", "unlisted"], []) [Str "Writing Extensible Functions"], Para [Str "To make your own functions extensible, follow this pattern:"], OrderedList (1, DefaultStyle, DefaultDelim) [[Plain [Strong [Str "Define a base"], Str " with a catch-all wildcard — this provides default behavior."]], [Plain [Strong [Str "Extend without a catch-all"], Str " — your new alternatives are prepended; the base stays as fallback."]]], CodeBlock ("", ["milang"], []) "-- Base definition (has catch-all)
describe val = val -> _ = \"something\"

-- Extension (no catch-all — chains)
Shape = {Circle radius; Rect width height}
describe val = val -> Circle = \"a circle\"; Rect = \"a rectangle\"

main world =
  world.io.println (describe (Circle 5))
  world.io.println (describe (Rect 3 4))
  world.io.println (describe 42)
", CodeBlock ("", [""], []) "a circle
a rectangle
something
", Para [Str "If you include a catch-all in an extension, it fully replaces the base —", SoftBreak, Str "use this when you genuinely want to override all behavior."]]