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
", CodeBlock ("", [""], []) "native codegen: unbound variable: fromMaybe
", Para [Str "Run this from your terminal:"], CodeBlock ("", ["bash"], []) "# With no arguments
./milang run hello_argv.mi
# Expected output: Hello, World!

# With an argument
./milang run hello_argv.mi \"Universe\"
# Expected output: Hello, Universe!
", Para [Str "This example shows several concepts:"], BulletList [[Plain [Code ("", [], []) "world.argv", Str ": A list of strings from the command line."]], [Plain [Code ("", [], []) "at'", Str ": A prelude function to safely get an element from a list by index. It returns a ", Code ("", [], []) "Maybe", Str " value. (", Code ("", [], []) "at'", Str " takes index first; ", Code ("", [], []) "at", Str " takes list first for use as an operator: ", Code ("", [], []) "xs `at` 1", Str ")."]], [Plain [Code ("", [], []) "fromMaybe", Str ": A prelude function that unwraps a ", Code ("", [], []) "Maybe", Str ", returning a default value if ", Code ("", [], []) "Nothing", Str "."]]], Para [Str "This pattern of using helpers to safely extract information is common in Milang."], Header 2 ("script-mode-quick-experiments", ["unnumbered", "unlisted"], []) [Str "Script Mode (quick experiments)"], Para [Str "When a file does not define ", Code ("", [], []) "main", Str " that takes a parameter, ", Code ("", [], []) "milang run", Str " executes in script mode: every top-level binding is evaluated and printed. This is ideal for short tests and REPL-style exploration."], CodeBlock ("", ["milang"], []) "x = 6 * 7
y = x + 1
", CodeBlock ("", [""], []) "_S6038675916313212016_eq = <closure>
_S6038675916313212016_if = <closure>
_S6038675916313212016_truthy = <closure>
_S6038675916313212016_toString = <closure>
_S6038675916313212016_id = <closure>
_S6038675916313212016_const = <closure>
_S6038675916313212016_flip = <closure>
_S6038675916313212016_null = <closure>
_S6038675916313212016_len = <closure>
_S6038675916313212016_head = <closure>
_S6038675916313212016_default = <closure>
_S6038675916313212016_tail = <closure>
_S6038675916313212016_fold = <closure>
_S6038675916313212016_map = <closure>
_S6038675916313212016_filter = <closure>
_S6038675916313212016_concat = <closure>
_S6038675916313212016_push = <closure>
_S6038675916313212016_at = <closure>
_S6038675916313212016_at' = <closure>
_S6038675916313212016_sum = <closure>
_S6038675916313212016_product = <closure>
_S6038675916313212016_any = <closure>
_S6038675916313212016_all = <closure>
_S6038675916313212016_contains = <closure>
_S6038675916313212016_range = <closure>
_S6038675916313212016_range_helper = <closure>
_S6038675916313212016_zip = <closure>
_S6038675916313212016__zip_acc = <closure>
_S6038675916313212016_last = <closure>
_S6038675916313212016_init = <closure>
_S6038675916313212016__init_acc = <closure>
_S6038675916313212016_cons = <closure>
_S6038675916313212016_reverse = <closure>
_S6038675916313212016__foldRange = <closure>
_S6038675916313212016_take = <closure>
_S6038675916313212016__take_acc = <closure>
_S6038675916313212016__take_step = <closure>
_S6038675916313212016_drop = <closure>
_S6038675916313212016_drop_inner = <closure>
_S6038675916313212016_enumerate = <closure>
_S6038675916313212016_join = <closure>
_S6038675916313212016_abs = <closure>
_S6038675916313212016_neg = <closure>
_S6038675916313212016_min = <closure>
_S6038675916313212016_max = <closure>
_S6038675916313212016_not = <closure>
_S6038675916313212016_|> = <closure>
_S6038675916313212016_>> = <closure>
_S6038675916313212016_<< = <closure>
_S6038675916313212016_&& = <closure>
_S6038675916313212016_|| = <closure>
_S6038675916313212016_flatMap = <closure>
_S6038675916313212016_foldRight = <closure>
_S6038675916313212016__foldRight_cps = <closure>
_S6038675916313212016_scanLeft = <closure>
_S6038675916313212016__scanLeft_acc = <closure>
_S6038675916313212016_unfold = <closure>
_S6038675916313212016__unfold_acc = <closure>
_S6038675916313212016__unfold_step_acc = <closure>
_S6038675916313212016_partition_step = <closure>
_S6038675916313212016_partition_yes = <closure>
_S6038675916313212016_partition_no = <closure>
_S6038675916313212016_partition = <closure>
_S6038675916313212016_iterate = <closure>
_S6038675916313212016_curry = <closure>
_S6038675916313212016_uncurry = <closure>
_S6038675916313212016_on = <closure>
_S6038675916313212016_fst = <closure>
_S6038675916313212016_snd = <closure>
_S6038675916313212016_sortBy = <closure>
_S6038675916313212016__sortBy_check = <closure>
_S6038675916313212016__mergeSort = <closure>
_S6038675916313212016__mergeSort_do = <closure>
_S6038675916313212016__mergeSort_split = <closure>
_S6038675916313212016__mergeSort_merge = <closure>
_S6038675916313212016_sort = <closure>
_S6038675916313212016_nubBy = <closure>
_S6038675916313212016__nubBy_acc = <closure>
_S6038675916313212016_nub = <closure>
_S6038675916313212016_groupBy = <closure>
_S6038675916313212016__groupBy_acc = <closure>
_S6038675916313212016_startsWith = <closure>
_S6038675916313212016_endsWith = <closure>
_S6038675916313212016_words = <closure>
_S6038675916313212016_lines = <closure>
_S6038675916313212016_unwords = <closure>
_S6038675916313212016_unlines = <closure>
_S6038675916313212016___AstNode = _module_ {App = <closure>, Var = <closure>, Lam = <closure>}
_S6038675916313212016__monad_then = <closure>
_S6038675916313212016__monad_bind = <closure>
_S6038675916313212016__monad_go = <closure>
_S6038675916313212016_monad = <closure>
_S6038675916313212016__maybe_bind = <closure>
_S6038675916313212016_maybe = <closure>
_S6038675916313212016_values = <closure>
_S6038675916313212016__collect_go = <closure>
_S6038675916313212016__collect_acc = <closure>
_S6038675916313212016_charAt = <closure>
_S6038675916313212016_fieldNames = <closure>
_S6038675916313212016_fields = <closure>
_S6038675916313212016_gc_manage = <closure>
_S6038675916313212016_getField = <closure>
_S6038675916313212016_indexOf = <closure>
_S6038675916313212016_replace = <closure>
_S6038675916313212016_setField = <closure>
_S6038675916313212016_slice = <closure>
_S6038675916313212016_split = <closure>
_S6038675916313212016_strlen = <closure>
_S6038675916313212016_tag = <closure>
_S6038675916313212016_toFloat = <closure>
_S6038675916313212016_toInt = <closure>
_S6038675916313212016_toLower = <closure>
_S6038675916313212016_toUpper = <closure>
_S6038675916313212016_trim = <closure>
charAt = <closure>
fieldNames = <closure>
fields = <closure>
gc_manage = <closure>
getField = <closure>
indexOf = <closure>
replace = <closure>
setField = <closure>
slice = <closure>
split = <closure>
strlen = <closure>
tag = <closure>
toFloat = <closure>
toInt = <closure>
toLower = <closure>
toUpper = <closure>
trim = <closure>
build =  {target = c, os = linux, arch = x86_64}
x = 42
y = 43
", Para [Str "Script-mode output prints name/value pairs for top-level bindings (prelude/internal bindings are hidden)."], Header 2 ("printing-non-strings-and-maybe-values", ["unnumbered", "unlisted"], []) [Str "Printing non-strings and Maybe values"], Para [Str "Use ", Code ("", [], []) "toString", Str " to render non-string values. Many standard library functions return ", Code ("", [], []) "Maybe", Str " to handle operations that might fail, like converting a string to a number. For example, ", Code ("", [], []) "toInt", Str " returns ", Code ("", [], []) "Just(number)", Str " on success and ", Code ("", [], []) "Nothing", Str " on failure."], Para [Str "Use ", Code ("", [], []) "toString", Str " to safely print these ", Code ("", [], []) "Maybe", Str " values."], CodeBlock ("", ["milang"], []) "main world =
  world.io.println (toString (toInt \"42\"))
  world.io.println (toString (toInt \"abc\"))
", CodeBlock ("", [""], []) "Just(42)
Nothing
", Para [Str "This will print:"], CodeBlock ("", ["text"], []) "Just(42)
Nothing
", Para [Str "The ", Code ("", [], []) "Maybe", Str " type is how Milang handles optional values, avoiding nulls and making error handling more explicit. You can use ", Link ("", [], []) [Str "pattern matching"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/single/src/language/pattern-matching.md#pattern-matching", ""), Str " to safely unwrap these values."], Header 2 ("compiling-to-c", ["unnumbered", "unlisted"], []) [Str "Compiling to C"], Para [Str "Emit the generated C and compile it:"], CodeBlock ("", ["bash"], []) "./milang compile hello.mi hello.c
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
", Para [Str "Bindings persist across lines; you may rethink and refine definitions live. Many common functions like ", Code ("", [], []) "map", Str ", ", Code ("", [], []) "filter", Str ", and ", Code ("", [], []) "fold", Str " are available automatically because they are part of the prelude."], Header 2 ("next-steps", ["unnumbered", "unlisted"], []) [Str "Next Steps"], BulletList [[Plain [Str "Read the full ", Link ("", [], []) [Str "syntax cheatsheet"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/single/src/language/cheatsheet.md#milang-syntax-cheatsheet", ""), Str "."]], [Plain [Str "Inspect reduction with ", Code ("", [], []) "./milang reduce", Str " (see ", Link ("", [], []) [Str "Partial Evaluation"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/single/src/language/partial-eval.md#partial-evaluation", ""), Str ")."]], [Plain [Str "Try the larger examples in the repository root."]]]]