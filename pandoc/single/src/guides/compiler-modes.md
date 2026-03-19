[Header 1 ("compiler-modes", [], []) [Str "Compiler Modes"], Para [Str "The ", Code ("", [], []) "milang", Str " binary supports several commands. Run ", Code ("", [], []) "milang --help", Str " for an overview or ", Code ("", [], []) "milang <command> --help", Str " for per-command options."], Header 2 ("milang-run", ["unnumbered", "unlisted"], []) [Code ("", [], []) "milang run"], CodeBlock ("", ["bash"], []) "milang run file.mi
milang run file.mi arg1 arg2      # pass arguments to the compiled program
milang run --keep-c file.mi       # keep the generated _core.c file after execution
", Para [Str "The most common command. It parses the file, resolves imports, partially evaluates, generates C, compiles with ", Code ("", [], []) "gcc -O2", Str ", runs the resulting binary, and cleans up temporary files."], Para [Str "If the file defines ", Code ("", [], []) "main world = ...", Str ", the program runs as a normal executable and ", Code ("", [], []) "main", Str "'s return value becomes the process exit code. If there is no ", Code ("", [], []) "main", Str " binding with a parameter, milang runs in ", Strong [Str "script mode"], Str " (see below)."], Header 3 ("example", ["unnumbered", "unlisted"], []) [Str "Example"], CodeBlock ("", ["milang"], []) "main world =
  world.io.println \"running!\"
", CodeBlock ("", [""], []) "running!
", Header 2 ("milang-compile", ["unnumbered", "unlisted"], []) [Code ("", [], []) "milang compile"], CodeBlock ("", ["bash"], []) "milang compile file.mi              # writes file.c
milang compile file.mi -o output.c  # writes output.c
milang compile file.mi -o -         # prints C to stdout
", Para [Str "Emits a self-contained C source file. The milang runtime is embedded in the output, so the generated file has no external dependencies beyond the standard C library."], Para [Str "Compile it yourself:"], CodeBlock ("", ["bash"], []) "gcc output.c -o program
./program
", Para [Str "This is useful when you want to inspect the generated code, cross-compile, or integrate milang output into a larger C project."], Header 2 ("milang-reduce", ["unnumbered", "unlisted"], []) [Code ("", [], []) "milang reduce"], CodeBlock ("", ["bash"], []) "milang reduce file.mi               # fully reduced AST (with prelude)
milang reduce --no-reduce file.mi   # parsed AST only (no reduction)
milang reduce --no-prelude file.mi  # reduced AST without prelude injection
milang reduce --json file.mi        # output structured JSON IR
milang reduce --json -o ir.json file.mi  # write JSON IR to a file
", Para [Str "The ", Code ("", [], []) "reduce", Str " command is the primary AST inspection tool. By default it shows the AST ", Strong [Str "after partial evaluation"], Str " with the prelude injected — this is exactly what the code generator sees. Any expression that was fully known at compile time has been reduced to a literal."], Para [Str "Flags control how much of the pipeline runs:"], Table ("", [], []) (Caption Nothing []) [(AlignDefault, (ColWidth 0.42857142857142855)), (AlignDefault, (ColWidth 0.5714285714285714))] (TableHead ("", [], []) [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Flag"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Effect"]]]]) [(TableBody ("", [], []) (RowHeadColumns 0) [] [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Emph [Str "(none)"]]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Parse → import resolution → prelude → reduce"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "--no-reduce"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Parse only; no imports or reduction (formerly ", Code ("", [], []) "dump", Str ")"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "--no-prelude"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Parse → imports → reduce, without prelude (formerly ", Code ("", [], []) "raw-reduce", Str ")"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "--json"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Emit a structured JSON IR instead of pretty-printed text"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "-o FILE"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Write output to ", Code ("", [], []) "FILE", Str " instead of stdout"]]]])] (TableFoot ("", [], []) []), Para [Str "Flags compose freely: ", Code ("", [], []) "--no-reduce --json", Str " gives you the raw parsed AST as JSON, and ", Code ("", [], []) "--no-prelude --json", Str " gives you the reduced AST of just your file without the standard library."], Header 3 ("example-1", ["unnumbered", "unlisted"], []) [Str "Example"], CodeBlock ("", ["milang"], []) "square x = x * 2
answer = square 21
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
square = <closure>
answer = 42
", Para [Str "Running ", Code ("", [], []) "milang reduce", Str " on this file shows ", Code ("", [], []) "answer = 42", Str " — the function call was evaluated at compile time."], Para [Str "Running ", Code ("", [], []) "milang reduce --no-reduce", Str " shows the unevaluated parse tree with ", Code ("", [], []) "answer = (square 21)", Str "."], Header 3 ("json-ir", ["unnumbered", "unlisted"], []) [Str "JSON IR"], Para [Code ("", [], []) "milang reduce --json", Str " serialises the reduced ", Code ("", [], []) "Expr", Str " tree to a stable JSON format. This is the foundation for building alternative backends — Python interpreters, C++ code generators, WASM targets, etc. — without needing to embed the Haskell compiler."], Para [Str "Every node has a ", Code ("", [], []) "\"tag\"", Str " field for dispatch. Example output for ", Code ("", [], []) "x = 13 * 7", Str ":"], CodeBlock ("", ["json"], []) "{
  \"tag\": \"namespace\",
  \"bindings\": [{
    \"domain\": \"value\",
    \"name\": \"x\",
    \"params\": [],
    \"body\": {
      \"tag\": \"binop\",
      \"op\": \"*\",
      \"left\":  { \"tag\": \"int\", \"value\": 13 },
      \"right\": { \"tag\": \"int\", \"value\": 7 }
    }
  }]
}
", Para [Str "After full reduction, ", Code ("", [], []) "--json", Str " produces ", Code ("", [], []) "{ \"tag\": \"int\", \"value\": 91 }", Str " for the ", Code ("", [], []) "x", Str " binding body."], Header 3 ("backward-compatible-aliases", ["unnumbered", "unlisted"], []) [Str "Backward-compatible aliases"], Para [Code ("", [], []) "milang dump", Str " and ", Code ("", [], []) "milang raw-reduce", Str " are kept as aliases:"], CodeBlock ("", ["bash"], []) "milang dump file.mi          # same as: milang reduce --no-reduce
milang raw-reduce file.mi    # same as: milang reduce --no-prelude
", Header 2 ("milang-pin", ["unnumbered", "unlisted"], []) [Code ("", [], []) "milang pin"], CodeBlock ("", ["bash"], []) "milang pin file.mi
", Para [Str "Finds all URL imports in the file, fetches them, computes a Merkle hash (SHA-256 of the content plus all transitive sub-imports), and rewrites the source file to include the hash:"], Para [Str "Before:"], CodeBlock ("", ["milang"], []) "utils = import \"https://example.com/utils.mi\"
", Para [Str "After:"], CodeBlock ("", ["milang"], []) "utils = import' \"https://example.com/utils.mi\" ({sha256 = \"a1b2c3...\"})
", Para [Str "This ensures that the imported code hasn't changed since you last pinned it. If the content changes, the compiler will report a hash mismatch and refuse to proceed."], Header 2 ("milang-repl", ["unnumbered", "unlisted"], []) [Code ("", [], []) "milang repl"], CodeBlock ("", ["bash"], []) "milang repl
", Para [Str "Starts an interactive REPL where you can evaluate expressions and define bindings. See the ", Link ("", [], []) [Str "REPL"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/single/src/guides/repl.md#repl", ""), Str " chapter for details."], Header 2 ("script-mode", ["unnumbered", "unlisted"], []) [Str "Script Mode"], Para [Str "When a ", Code ("", [], []) ".mi", Str " file has no ", Code ("", [], []) "main", Str " binding that takes a parameter, ", Code ("", [], []) "milang run", Str " operates in ", Strong [Str "script mode"], Str ": it evaluates every top-level binding and prints each name-value pair."], CodeBlock ("", ["milang"], []) "a = 2 + 3
b = a * a
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
a = 5
b = 25
greeting = hello
", Para [Str "Script mode is ideal for quick calculations and testing small snippets. Prelude definitions are automatically hidden from the output."], Header 2 ("security-flags", ["unnumbered", "unlisted"], []) [Str "Security Flags"], Para [Str "Milang supports flags to restrict what imported code can do (note: these flags are not currently implemented in the core compiler; see .github/ROADMAP.md):"], Table ("", [], []) (Caption Nothing []) [(AlignDefault, (ColWidth 0.42857142857142855)), (AlignDefault, (ColWidth 0.5714285714285714))] (TableHead ("", [], []) [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Flag"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Effect"]]]]) [(TableBody ("", [], []) (RowHeadColumns 0) [] [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "--no-ffi"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Disables all C FFI imports (no ", Code ("", [], []) ".h", Str " files can be imported)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "--no-remote-ffi"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Disallows C FFI for remote (URL) imports specifically"]]]])] (TableFoot ("", [], []) []), Para [Str "These flags are useful when running untrusted code. A URL import might try to ", Code ("", [], []) "import \"/usr/include/stdlib.h\"", Str " and call arbitrary C functions — ", Code ("", [], []) "--no-remote-ffi", Str " prevents this while still allowing your own local FFI usage."], Header 2 ("summary", ["unnumbered", "unlisted"], []) [Str "Summary"], Table ("", [], []) (Caption Nothing []) [(AlignDefault, ColWidthDefault), (AlignDefault, ColWidthDefault)] (TableHead ("", [], []) [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Command"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "What it does"]]]]) [(TableBody ("", [], []) (RowHeadColumns 0) [] [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "run file.mi"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Compile and execute"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "compile file.mi [-o out.c]"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Emit standalone C"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "reduce file.mi [flags]"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Inspect AST (pretty-print or JSON)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "pin file.mi"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Fetch URL imports, write SHA-256 hashes"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "repl"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Interactive evaluation"]]]])] (TableFoot ("", [], []) [])]