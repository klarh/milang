[Header 1 ("io--the-world", [], []) [Str "IO & the World"], Para [Str "Milang uses a capability-based IO model. Side effects are not global — they flow through an explicit ", Code ("", [], []) "world", Str " record that the runtime passes to ", Code ("", [], []) "main", Str ". If a function never receives ", Code ("", [], []) "world", Str " (or a sub-record of it), it cannot perform IO."], Header 2 ("hello-world", ["unnumbered", "unlisted"], []) [Str "Hello World"], CodeBlock ("", ["milang"], []) "main world =
  world.io.println \"Hello, world!\"
", CodeBlock ("", [""], []) "Hello, world!
", Para [Code ("", [], []) "main", Str " is the program entry point. It receives ", Code ("", [], []) "world", Str " and its return value becomes the process exit code."], Header 2 ("the-world-record", ["unnumbered", "unlisted"], []) [Str "The World Record"], Para [Code ("", [], []) "world", Str " is a record containing sub-records for different capabilities:"], Table ("", [], []) (Caption Nothing []) [(AlignDefault, ColWidthDefault), (AlignDefault, ColWidthDefault)] (TableHead ("", [], []) [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Path"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Description"]]]]) [(TableBody ("", [], []) (RowHeadColumns 0) [] [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "world.io"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Console IO (println, print, readLine)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "world.fs.read"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Read-only filesystem (file, exists)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "world.fs.write"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Write filesystem (file, append, remove)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "world.fs"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Full filesystem access (read + write)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "world.process"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Process execution and exit"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "world.argv"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Command-line arguments (pure — no effect)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "world.getEnv"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Read environment variables"]]]])] (TableFoot ("", [], []) []), Header 2 ("console-io", ["unnumbered", "unlisted"], []) [Str "Console IO"], CodeBlock ("", ["milang"], []) "world.io.println msg          -- print with trailing newline
world.io.print msg            -- print without newline
line = world.io.readLine      -- read one line from stdin
", Header 2 ("file-io", ["unnumbered", "unlisted"], []) [Str "File IO"], Para [Str "File operations are split by capability: ", Code ("", [], []) "world.fs.read", Str " for reading and", SoftBreak, Code ("", [], []) "world.fs.write", Str " for writing. This enables fine-grained trait annotations."], CodeBlock ("", ["milang"], []) "content = world.fs.read.file \"data.txt\"
world.fs.write.file \"out.txt\" content
world.fs.write.append \"log.txt\" \"new line\\n\"
exists = world.fs.read.exists \"data.txt\"     -- returns 1 or 0
world.fs.write.remove \"tmp.txt\"
", Header 2 ("process", ["unnumbered", "unlisted"], []) [Str "Process"], CodeBlock ("", ["milang"], []) "output = world.process.exec \"ls -la\"      -- run shell command, return output
world.process.exit 1                       -- exit immediately with status code
", Header 2 ("command-line-arguments-and-environment", ["unnumbered", "unlisted"], []) [Str "Command-Line Arguments and Environment"], Para [Code ("", [], []) "world.argv", Str " is an inert list — it does not perform IO, so it is always available:"], CodeBlock ("", ["milang"], []) "main world =
  world.io.println (len world.argv)
", Para [Code ("", [], []) "world.getEnv", Str " reads an environment variable by name:"], CodeBlock ("", ["milang"], []) "home = world.getEnv \"HOME\"
", Header 2 ("capability-restriction", ["unnumbered", "unlisted"], []) [Str "Capability Restriction"], Para [Str "Because capabilities are just record ", Code ("", [], []) "fields", Str ", you can restrict what a helper function can do by passing only the sub-record it needs:"], CodeBlock ("", ["milang"], []) "greet io = io.println \"hello from restricted IO\"

main world =
  greet world.io
", CodeBlock ("", [""], []) "hello from restricted IO
", Para [Code ("", [], []) "greet", Str " receives ", Code ("", [], []) "world.io", Str " and can print, but it structurally cannot access ", Code ("", [], []) "world.process", Str " — there is no way for it to execute shell commands or exit the process."], Header 2 ("exit-code", ["unnumbered", "unlisted"], []) [Str "Exit Code"], Para [Str "The return value of ", Code ("", [], []) "main", Str " is used as the process exit code. An integer is used directly; any non-integer value (record, string, etc.) defaults to exit code 0."], CodeBlock ("", ["milang"], []) "main world =
  world.io.println \"exiting with code 0\"
", CodeBlock ("", [""], []) "exiting with code 0
", Header 2 ("script-mode", ["unnumbered", "unlisted"], []) [Str "Script Mode"], Para [Str "When a file has no ", Code ("", [], []) "main", Str " binding that takes a parameter, milang runs in script mode: every top-level binding is evaluated and printed."], CodeBlock ("", ["milang"], []) "x = 6 * 7
y = x + 1
greeting = \"hello\"
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
greeting = hello
", Para [Str "This is useful for quick calculations and exploring the language without writing a full ", Code ("", [], []) "main", Str " function."], Header 2 ("auto-monad-spine", ["unnumbered", "unlisted"], []) [Str "Auto-Monad Spine"], Para [Str "You do not need monads or do-notation in milang. The compiler automatically tracks which expressions are ", Emph [Code ("", [], []) "world", Str "-tainted"], Str " (they transitively reference ", Code ("", [], []) "world", Str "). Impure expressions are guaranteed to execute in the order they appear in the source. Pure expressions can float freely, opening the door for future optimizations. The result is imperative-looking code that is safe and predictable."]]