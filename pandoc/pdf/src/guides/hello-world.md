[Header 1 ("hello-world", [], []) [Str "Hello World"], Para [Str "This guide walks through creating, running, and compiling your first Milang program and explains common variants useful when learning the language."], Header 2 ("your-first-program", ["unnumbered", "unlisted"], []) [Str "Your First Program"], Para [Str "Create a file called hello.mi with this content:"], CodeBlock ("", ["milang"], []) "main world =
  world.io.println \"Hello, Milang!\"
", CodeBlock ("", [""], []) "Hello, Milang!
", Para [Str "Run it with the bundled binary:"], CodeBlock ("", ["bash"], []) "./milang run hello.mi
", Para [Str "Expected output:"], CodeBlock ("", [""], []) "Hello, Milang!
", Header 3 ("what-main-and-world-mean", ["unnumbered", "unlisted"], []) [Str "What ", Code ("", [], []) "main", Str " and ", Code ("", [], []) "world", Str " mean"], BulletList [[Plain [Code ("", [], []) "main", Str " is the program entry point by convention (not a language keyword)."]], [Plain [Code ("", [], []) "world", Str " is an explicit record that carries runtime capabilities: ", Code ("", [], []) "world.io", Str " (console and file IO), ", Code ("", [], []) "world.process", Str " (exec/exit), ", Code ("", [], []) "world.argv", Str ", and helpers like ", Code ("", [], []) "getEnv", Str "."]], [Plain [Str "Only code that receives the appropriate part of ", Code ("", [], []) "world", Str " can perform the corresponding effects — pass only what you need to follow the principle of least privilege."]]], Header 2 ("printing-and-helpers", ["unnumbered", "unlisted"], []) [Str "Printing and Helpers"], Para [Code ("", [], []) "println", Str " appends a newline; ", Code ("", [], []) "print", Str " does not. Prefer small helpers that accept only the sub-record they need:"], CodeBlock ("", ["milang"], []) "greet io name = io.println (\"Hello, \" + name + \"!\")

main world =
  greet world.io \"Alice\"
", CodeBlock ("", [""], []) "Hello, Alice!
", Para [Str "This makes ", Code ("", [], []) "greet", Str " unable to access process or filesystem capabilities."], Header 2 ("handling-command-line-arguments", ["unnumbered", "unlisted"], []) [Str "Handling Command-Line Arguments"], Para [Str "A more advanced \"Hello World\" might greet someone by name, using command-line arguments. The ", Code ("", [], []) "world.argv", Str " list contains the arguments. The following example, which you can save as ", Code ("", [], []) "hello_argv.mi", Str ", demonstrates this. It uses a helper function to safely get an argument or fall back to a default value."], CodeBlock ("", ["milang"], []) "-- main entrypoint
main world =
  name = fromMaybe \"World\" (at' 1 world.argv)
  world.io.println (\"Hello, \" + name + \"!\")
", CodeBlock ("", [""], []) "Hello, World!
", Para [Str "Run this from your terminal:"], CodeBlock ("", ["bash"], []) "# With no arguments
./milang run hello_argv.mi
# Expected output: Hello, World!

# With an argument
./milang run hello_argv.mi \"Universe\"
# Expected output: Hello, Universe!
", Para [Str "This example shows several concepts:"], BulletList [[Plain [Code ("", [], []) "world.argv", Str ": A list of strings from the command line."]], [Plain [Code ("", [], []) "at'", Str ": A prelude function to safely get an element from a list by index. It returns a ", Code ("", [], []) "Maybe", Str " value. (", Code ("", [], []) "at'", Str " takes index first; ", Code ("", [], []) "at", Str " takes list first for use as an operator: ", Code ("", [], []) "xs `at` 1", Str ")."]], [Plain [Code ("", [], []) "fromMaybe", Str ": A prelude function that unwraps a ", Code ("", [], []) "Maybe", Str ", returning a default value if ", Code ("", [], []) "Nothing", Str "."]]], Para [Str "This pattern of using helpers to safely extract information is common in Milang."], Header 2 ("script-mode-quick-experiments", ["unnumbered", "unlisted"], []) [Str "Script Mode (quick experiments)"], Para [Str "When a file does not define ", Code ("", [], []) "main", Str " that takes a parameter, ", Code ("", [], []) "milang run", Str " executes in script mode: every top-level binding is evaluated and printed. This is ideal for short tests and REPL-style exploration."], CodeBlock ("", ["milang"], []) "x = 6 * 7
y = x + 1
", CodeBlock ("", [""], []) "x = 42
y = 43
", Para [Str "Script-mode output prints name/value pairs for top-level bindings (prelude/internal bindings are hidden)."], Header 2 ("printing-non-strings-and-maybe-values", ["unnumbered", "unlisted"], []) [Str "Printing non-strings and Maybe values"], Para [Str "Use ", Code ("", [], []) "toString", Str " to render non-string values. Many standard library functions return ", Code ("", [], []) "Maybe", Str " to handle operations that might fail, like converting a string to a number. For example, ", Code ("", [], []) "toInt", Str " returns ", Code ("", [], []) "Just(number)", Str " on success and ", Code ("", [], []) "Nothing", Str " on failure."], Para [Str "Use ", Code ("", [], []) "toString", Str " to safely print these ", Code ("", [], []) "Maybe", Str " values."], CodeBlock ("", ["milang"], []) "main world =
  world.io.println (toString (toInt \"42\"))
  world.io.println (toString (toInt \"abc\"))
", CodeBlock ("", [""], []) "Just(42)
Nothing
", Para [Str "This will print:"], CodeBlock ("", ["text"], []) "Just(42)
Nothing
", Para [Str "The ", Code ("", [], []) "Maybe", Str " type is how Milang handles optional values, avoiding nulls and making error handling more explicit. You can use ", Link ("", [], []) [Str "pattern matching"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/pdf/src/language/pattern-matching.md#pattern-matching", ""), Str " to safely unwrap these values."], Header 2 ("compiling-to-c", ["unnumbered", "unlisted"], []) [Str "Compiling to C"], Para [Str "Emit the generated C and compile it:"], CodeBlock ("", ["bash"], []) "./milang compile hello.mi hello.c
gcc hello.c -o hello
./hello
", Para [Str "The C file embeds the milang runtime; you only need a standard C toolchain."], Header 2 ("using-the-repl", ["unnumbered", "unlisted"], []) [Str "Using the REPL"], Para [Str "Start the REPL for interactive experimentation:"], CodeBlock ("", ["bash"], []) "./milang repl
", Para [Str "Example session:"], CodeBlock ("", ["text"], []) "> 2 + 3
5
> f x = x * x
> f 8
64
> map f [1, 2, 3, 4]
[1, 4, 9, 16]
", Para [Str "Bindings persist across lines; you may rethink and refine definitions live. Many common functions like ", Code ("", [], []) "map", Str ", ", Code ("", [], []) "filter", Str ", and ", Code ("", [], []) "fold", Str " are available automatically because they are part of the prelude."], Header 2 ("next-steps", ["unnumbered", "unlisted"], []) [Str "Next Steps"], BulletList [[Plain [Str "Read the full ", Link ("", [], []) [Str "syntax cheatsheet"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/pdf/src/language/cheatsheet.md#milang-syntax-cheatsheet", ""), Str "."]], [Plain [Str "Inspect reduction with ", Code ("", [], []) "./milang reduce", Str " (see ", Link ("", [], []) [Str "Partial Evaluation"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/pdf/src/language/partial-eval.md#partial-evaluation", ""), Str ")."]], [Plain [Str "Try the larger examples in the repository root."]]]]