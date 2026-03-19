[Header 1 ("partial-evaluation", [], []) [Str "Partial Evaluation"], Para [Str "Partial evaluation is milang's core compilation model. There is no separate", SoftBreak, Str "optimisation pass — the compiler itself evaluates every expression whose inputs", SoftBreak, Str "are known at compile time and emits C code only for what remains. The result is", SoftBreak, Str "that high-level abstractions (helper functions, configuration records, computed", SoftBreak, Str "constants) often carry ", Strong [Str "zero runtime cost"], Str "."], Header 2 ("how-it-works", ["unnumbered", "unlisted"], []) [Str "How it works"], Para [Str "When the compiler processes a binding it walks the expression tree with a", SoftBreak, Str "recursive reducer (", Code ("", [], []) "reduceD", Str "). At each node it checks whether the operands are", SoftBreak, Emph [Str "concrete"], Str " — literal integers, floats, strings, lambdas, or records whose", SoftBreak, Code ("", [], []) "fields", Str " are themselves concrete. If they are, the expression is evaluated", SoftBreak, Str "immediately and replaced by its result. If any operand is unknown (a function", SoftBreak, Str "parameter, an IO result, etc.) the expression is left as ", Emph [Str "residual code"], Str " for", SoftBreak, Str "the C back-end to emit."], CodeBlock ("", ["milang"], []) "-- Fully reduced at compile time:
x = 6 * 7              -- becomes: x = 42
f a = a * 2
y = f 21                -- becomes: y = 42

-- Stays as residual code (parameter unknown):
double a = a * 2        -- emitted as a C function
", Header 3 ("scc-dependency-analysis", ["unnumbered", "unlisted"], []) [Str "SCC dependency analysis"], Para [Str "Bindings are sorted into ", Emph [Str "strongly connected components"], Str " so that each group is", SoftBreak, Str "reduced in dependency order. Mutually-recursive bindings land in the same SCC", SoftBreak, Str "and are handled together."], Header 3 ("depth-limited-recursion", ["unnumbered", "unlisted"], []) [Str "Depth-limited recursion"], Para [Str "Recursive functions are unrolled only when every argument is concrete, and", SoftBreak, Str "reduction is capped at a fixed depth (128 steps). This prevents the compiler", SoftBreak, Str "from looping on unbounded recursion while still collapsing finite recursive", SoftBreak, Str "computations at compile time."], Header 2 ("zero-cost-abstractions", ["unnumbered", "unlisted"], []) [Str "Zero-cost abstractions"], Para [Str "Because the reducer runs before code generation, any abstraction that is fully", SoftBreak, Str "known at compile time disappears entirely from the output:"], CodeBlock ("", ["milang"], []) "-- Configuration record — reduced away at compile time
config = {width = 800; height = 600}
pixels = config.width * config.height
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
config =  {width = 800, height = 600}
pixels = 480000
", Para [Str "The binding ", Code ("", [], []) "pixels", Str " is reduced to the integer ", Code ("", [], []) "480000", Str " before any C code is", SoftBreak, Str "generated. No record allocation, no field lookup — just a constant."], Header 2 ("inspecting-the-reducer-output", ["unnumbered", "unlisted"], []) [Str "Inspecting the reducer output"], Para [Str "milang ships two commands for inspecting what the compiler sees:"], CodeBlock ("", ["bash"], []) "milang dump file.mi      -- parsed AST (before reduction)
milang reduce file.mi    -- AST after partial evaluation (what codegen sees)
", Para [Str "Comparing the two on the same file shows exactly which expressions were", SoftBreak, Str "collapsed and which remain as residual code. This is the primary tool for", SoftBreak, Str "understanding compile-time behaviour."], Header 2 ("what-stays-as-residual-code", ["unnumbered", "unlisted"], []) [Str "What stays as residual code"], Para [Str "Anything that depends on a value unknown at compile time is left for the C", SoftBreak, Str "back-end:"], CodeBlock ("", ["milang"], []) "main world =
  line = world.io.readLine    -- runtime IO — cannot reduce
  world.io.println line       -- emitted as C call
", Para [Str "Function parameters, IO results, and any expression transitively depending on", SoftBreak, Str "them are residual. Everything else is reduced."], RawBlock (Format "html") "<!-- Sized types and partial evaluation

When the reducer encounters expressions involving sized numeric types the
following notes explain the current behaviour and constraints:

- The width argument to `Int'`, `UInt'`, or `Float'` must be a compile-time
  constant. The reducer requires concrete `n` values to reason about types;
  expressions like `Int' (4 * 8)` will reduce to `Int' 32` when the arithmetic
  inside the type expression is concrete.

- Type aliases such as `Int = Int' 0` are treated as syntactic sugar and are
  resolved during reduction; they do not remain at runtime.

- Sized types are both type-level annotations and value constructors. The
  reducer performs width-aware arithmetic: fixed-width types (`Int' 8`, etc.)
  clamp/wrap at their specified width, while arbitrary-precision types
  (`Int' 0`, bare integers) use Haskell's unbounded `Integer` type.

-->"]