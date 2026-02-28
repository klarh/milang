[Header 1 ("traits--effects-", [], []) [Str "Traits & Effects (", Code ("", [], []) ":~", Str ")"], Para [Str "The ", Code ("", [], []) ":~", Str " annotation domain attaches trait or effect information to a binding. It describes ", Emph [Str "what capabilities"], Str " a function uses — console IO, file reads, process execution, and so on. Traits annotations are orthogonal to type annotations (", Code ("", [], []) "::", Str ") and can coexist on the same binding."], Header 2 ("syntax", ["unnumbered", "unlisted"], []) [Str "Syntax"], CodeBlock ("", ["milang"], []) "name :~ traitsExpr
", Para [Str "The traits expression is typically a list of effect names:"], CodeBlock ("", ["milang"], []) "greet :~ [console]
greet world = world.io.println \"hello\"
", Header 2 ("effect-names", ["unnumbered", "unlisted"], []) [Str "Effect Names"], Table ("", [], []) (Caption Nothing []) [(AlignDefault, ColWidthDefault), (AlignDefault, ColWidthDefault)] (TableHead ("", [], []) [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Effect"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Capabilities covered"]]]]) [(TableBody ("", [], []) (RowHeadColumns 0) [] [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "console"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "println", Str ", ", Code ("", [], []) "print", Str ", ", Code ("", [], []) "readLine"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "fs.read"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "readFile", Str ", ", Code ("", [], []) "exists"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "fs.write"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "writeFile", Str ", ", Code ("", [], []) "appendFile", Str ", ", Code ("", [], []) "remove"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "exec"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "process.exec"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "env"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "getEnv"]]]])] (TableFoot ("", [], []) []), Para [Str "Use ", Code ("", [], []) "[]", Str " (empty list) or define a name bound to ", Code ("", [], []) "[]", Str " to declare a function as pure:"], CodeBlock ("", ["milang"], []) "pure :~ []

add :~ pure
add a b = a + b
", Header 2 ("defining-effect-groups", ["unnumbered", "unlisted"], []) [Str "Defining Effect Groups"], Para [Str "You can define reusable groups of effects:"], CodeBlock ("", ["milang"], []) "readonly :~ [console, fs.read]
readwrite :~ [console, fs.read, fs.write]
", Para [Str "Then reference those groups in other annotations."], Header 2 ("example", ["unnumbered", "unlisted"], []) [Str "Example"], CodeBlock ("", ["milang"], []) "distance :~ []
distance x1 y1 x2 y2 = (x2 - x1)**2 + (y2 - y1)**2

main world =
  world.io.println (distance 0 0 3 4)
", CodeBlock ("", [""], []) "25
", Header 2 ("combining-with-other-domains", ["unnumbered", "unlisted"], []) [Str "Combining with Other Domains"], Para [Str "All annotation domains can coexist on a single binding:"], CodeBlock ("", ["milang"], []) "add :? \"Add two numbers\"
add :: Num : Num : Num
add :~ []
add a b = a + b
", Header 2 ("current-status", ["unnumbered", "unlisted"], []) [Str "Current Status"], Para [Str "Trait annotations are parsed, stored, and ", Strong [Str "enforced"], Str " by the compiler. The compiler performs taint analysis: it tracks the ", Code ("", [], []) "world", Str " value and any names that transitively reference it (via aliasing or closures), then infers the effect set of every binding. If a function's inferred effects are not a subset of its declared traits, the compiler emits an error."], Para [Strong [Str "Functions without a ", Code ("", [], []) ":~", Str " annotation are assumed pure"], Str " (", Code ("", [], []) ":~ []", Str "). This means any function that performs IO must declare its effects. The only exception is ", Code ("", [], []) "main", Str ", which is implicitly granted all capabilities."], Para [Str "For example, declaring ", Code ("", [], []) ":~ []", Str " (pure) but calling ", Code ("", [], []) "world.io.println", Str " inside the body is a compile error — and so is omitting the annotation entirely:"], CodeBlock ("", ["milang"], []) "-- This is an error: no annotation, so assumed pure, but uses console
helper world = world.io.println \"oops\"

-- Fix: add trait annotation
helper :~ [console]
helper world = world.io.println \"ok\"
"]