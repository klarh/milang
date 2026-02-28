[Header 1 ("repl", [], []) [Str "REPL"], Para [Str "The milang REPL (Read-Eval-Print Loop) lets you evaluate expressions and define bindings interactively."], Header 2 ("starting-the-repl", ["unnumbered", "unlisted"], []) [Str "Starting the REPL"], CodeBlock ("", ["bash"], []) "./milang repl
", Para [Str "You'll see a ", Code ("", [], []) "λ>", Str " prompt. Type any expression and press Enter to evaluate it:"], CodeBlock ("", ["text"], []) "λ> 2 + 3
5
λ> \"hello\" + \" \" + \"world\"
\"hello world\"
", Para [Str "Type ", Code ("", [], []) ":q", Str " or press ", Strong [Str "Ctrl-D"], Str " to exit."], Header 2 ("defining-bindings", ["unnumbered", "unlisted"], []) [Str "Defining Bindings"], Para [Str "Bindings persist across inputs, so you can build up definitions incrementally:"], CodeBlock ("", ["text"], []) "λ> double x = x * 2
double = (\\x -> (x * 2))
λ> double 21
42
λ> quadruple x = double (double x)
quadruple = (\\x -> (double (double x)))
λ> quadruple 5
20
", Header 2 ("single-line-input", ["unnumbered", "unlisted"], []) [Str "Single-line Input"], Para [Str "The REPL reads one line at a time. Each binding must fit on a single line. Use semicolons to separate alternatives within a ", Code ("", [], []) "->", Str " pattern match:"], CodeBlock ("", ["text"], []) "λ> area s = s -> Circle = 3.14 * s.radius * s.radius; Rect = s.width * s.height
", Para [Str "Multi-line indented definitions must be written in a ", Code ("", [], []) ".mi", Str " file and loaded via ", Code ("", [], []) "milang run", Str "."], Header 2 ("prelude-functions", ["unnumbered", "unlisted"], []) [Str "Prelude Functions"], Para [Str "All standard prelude functions are available immediately — no imports needed:"], CodeBlock ("", ["text"], []) "λ> map (\\x = x * x) [1, 2, 3, 4, 5]
Cons {head = 1, tail = Cons {head = 4, tail = ...}}
λ> filter (\\x = x > 3) [1, 2, 3, 4, 5]
Cons {head = 4, tail = Cons {head = 5, tail = Nil {}}}
λ> fold (\\acc x = acc + x) 0 [1, 2, 3]
6
λ> len [10, 20, 30]
3
", BlockQuote [Para [Strong [Str "Note:"], Str " Lists are displayed as raw ", Code ("", [], []) "Cons", Str "/", Code ("", [], []) "Nil", Str " record expressions — the REPL shows the partially-evaluated AST, not a pretty-printed representation."]], Header 2 ("type-annotations", ["unnumbered", "unlisted"], []) [Str "Type Annotations"], Para [Str "You can add type annotations to bindings:"], CodeBlock ("", ["text"], []) "λ> x :: Int
λ> x = 42
x = 42
", Para [Str "The type is associated with the binding and checked when the value is defined."], Header 2 ("viewing-bindings", ["unnumbered", "unlisted"], []) [Str "Viewing Bindings"], Para [Str "Use ", Code ("", [], []) ":env", Str " to show all user-defined bindings (prelude bindings are hidden):"], CodeBlock ("", ["text"], []) "λ> double x = x * 2
double = (\\x -> (x * 2))
λ> :env
double = (\\x -> (x * 2))
", Header 2 ("how-it-works", ["unnumbered", "unlisted"], []) [Str "How It Works"], Para [Str "Each REPL input is:"], OrderedList (1, DefaultStyle, DefaultDelim) [[Plain [Str "Parsed as either a binding (namespace) or a bare expression"]], [Plain [Str "Reduced using the same partial evaluator as ", Code ("", [], []) "milang reduce"]], [Plain [Str "The reduced form is printed"]]], Para [Str "New bindings extend the accumulated environment for all subsequent inputs. This is a ", Strong [Str "pure partial evaluator"], Str " — there is no C compilation or gcc invocation in the REPL. Residuals (expressions that cannot be further reduced) are printed as-is."], Header 2 ("limitations", ["unnumbered", "unlisted"], []) [Str "Limitations"], BulletList [[Plain [Strong [Str "No IO"], Str " — the REPL evaluates pure expressions only. There is no ", Code ("", [], []) "world", Str " value available, so ", Code ("", [], []) "world.io.println", Str " and similar IO operations cannot be used."]], [Plain [Strong [Str "No imports"], Str " — ", Code ("", [], []) "import", Str " declarations are not supported in the REPL."]], [Plain [Strong [Str "No multi-line input"], Str " — each input must fit on a single line. Write multi-line programs in ", Code ("", [], []) ".mi", Str " files."]], [Plain [Strong [Str "No command history"], Str " — the up/down arrow keys do not recall previous inputs."]], [Plain [Strong [Str "Raw list output"], Str " — lists are printed as ", Code ("", [], []) "Cons", Str "/", Code ("", [], []) "Nil", Str " record expressions, not ", Code ("", [], []) "[1, 2, 3]", Str "."]]], Para [Str "For IO and imports, write a ", Code ("", [], []) ".mi", Str " file and use ", Code ("", [], []) "milang run", Str " instead."]]