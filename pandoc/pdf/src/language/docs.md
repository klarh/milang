[Header 1 ("documentation-", [], []) [Str "Documentation (", Code ("", [], []) ":?", Str ")"], Para [Str "The ", Code ("", [], []) ":?", Str " annotation domain attaches documentation to a binding. Doc expressions are ordinary milang expressions — usually strings or structured records — that the compiler stores as compile-time metadata. They do not affect runtime behavior."], Header 2 ("simple-string-docs", ["unnumbered", "unlisted"], []) [Str "Simple String Docs"], Para [Str "The most common form is a short description string:"], CodeBlock ("", ["milang"], []) "add :? \"Add two numbers\"
add a b = a + b
", Header 2 ("structured-docs", ["unnumbered", "unlisted"], []) [Str "Structured Docs"], Para [Str "For richer documentation, use a record with ", Code ("", [], []) "fields", Str " like ", Code ("", [], []) "summary", Str ", ", Code ("", [], []) "params", Str ", and ", Code ("", [], []) "returns", Str ":"], CodeBlock ("", ["milang"], []) "distance :? {
  summary = \"Squared distance between two points\"
  params = {
    x1 = \"First x coordinate\"
    y1 = \"First y coordinate\"
    x2 = \"Second x coordinate\"
    y2 = \"Second y coordinate\"
  }
  returns = \"The squared distance as a number\"
}
distance x1 y1 x2 y2 = (x2 - x1)**2 + (y2 - y1)**2
", Para [Str "The field names are not enforced — you can use whatever structure makes sense for your project."], Header 2 ("triple-quoted-string-docs", ["unnumbered", "unlisted"], []) [Str "Triple-Quoted String Docs"], Para [Str "For multi-line documentation, use triple-quoted strings. Margin stripping (based on the closing ", Code ("", [], []) "\"\"\"", Str " indentation) keeps the source tidy:"], CodeBlock ("", ["milang"], []) "greet :? \"\"\"
  Greet a person by name.
  Prints a friendly message to the console.
  \"\"\"
greet world name = world.io.println (\"Hello, \" + name + \"!\")
", Header 2 ("example", ["unnumbered", "unlisted"], []) [Str "Example"], CodeBlock ("", ["milang"], []) "add :? \"Add two numbers\"
add :: Num : Num : Num
add a b = a + b

distance :? {summary = \"Squared distance\"; returns = \"Num\"}
distance x1 y1 x2 y2 = (x2 - x1)**2 + (y2 - y1)**2

main world =
  world.io.println (add 3 4)
  world.io.println (distance 0 0 3 4)
", CodeBlock ("", [""], []) "7
25
", Para [Str "Doc annotations do not change execution — the output above is the same with or without ", Code ("", [], []) ":?", Str " lines."], Header 2 ("combining-all-five-domains", ["unnumbered", "unlisted"], []) [Str "Combining All Five Domains"], Para [Str "Every annotation domain can coexist on a single binding:"], CodeBlock ("", ["milang"], []) "distance :? \"Squared Euclidean distance\"
distance :: Num : Num : Num : Num : Num
distance :~ []
distance x1 y1 x2 y2 = (x2 - x1)**2 + (y2 - y1)**2
", Para [Str "The domains are ", Code ("", [], []) "=", Str " (value), ", Code ("", [], []) "::", Str " (type), ", Code ("", [], []) ":~", Str " (traits), ", Code ("", [], []) ":?", Str " (docs), and ", Code ("", [], []) ":!", Str " (parse). They are independent and can appear in any order before the value binding."], Header 2 ("future-milang-doc", ["unnumbered", "unlisted"], []) [Str "Future: ", Code ("", [], []) "milang doc"], Para [Str "A planned ", Code ("", [], []) "milang doc", Str " command will extract ", Code ("", [], []) ":?", Str " annotations from source files and generate reference documentation automatically."]]