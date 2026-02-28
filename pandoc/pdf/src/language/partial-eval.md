[Header 1 ("partial-evaluation", [], []) [Str "Partial Evaluation"], Para [Str "Partial evaluation is milang's core compilation model. There is no separate", SoftBreak, Str "optimisation pass — the compiler itself evaluates every expression whose inputs", SoftBreak, Str "are known at compile time and emits C code only for what remains. The result is", SoftBreak, Str "that high-level abstractions (helper functions, configuration records, computed", SoftBreak, Str "constants) often carry ", Strong [Str "zero runtime cost"], Str "."], Header 2 ("how-it-works", ["unnumbered", "unlisted"], []) [Str "How it works"], Para [Str "When the compiler processes a binding it walks the expression tree with a", SoftBreak, Str "recursive reducer (", Code ("", [], []) "reduceD", Str "). At each node it checks whether the operands are", SoftBreak, Emph [Str "concrete"], Str " — literal integers, floats, strings, lambdas, or records whose", SoftBreak, Code ("", [], []) "fields", Str " are themselves concrete. If they are, the expression is evaluated", SoftBreak, Str "immediately and replaced by its result. If any operand is unknown (a function", SoftBreak, Str "parameter, an IO result, etc.) the expression is left as ", Emph [Str "residual code"], Str " for", SoftBreak, Str "the C back-end to emit."], CodeBlock ("", ["milang"], []) "-- Fully reduced at compile time:
x = 6 * 7              -- becomes: x = 42
f a = a * 2
y = f 21                -- becomes: y = 42

-- Stays as residual code (parameter unknown):
double a = a * 2        -- emitted as a C function
", Header 3 ("scc-dependency-analysis", ["unnumbered", "unlisted"], []) [Str "SCC dependency analysis"], Para [Str "Bindings are sorted into ", Emph [Str "strongly connected components"], Str " so that each group is", SoftBreak, Str "reduced in dependency order. Mutually-recursive bindings land in the same SCC", SoftBreak, Str "and are handled together."], Header 3 ("depth-limited-recursion", ["unnumbered", "unlisted"], []) [Str "Depth-limited recursion"], Para [Str "Recursive functions are unrolled only when every argument is concrete, and", SoftBreak, Str "reduction is capped at a fixed depth (128 steps). This prevents the compiler", SoftBreak, Str "from looping on unbounded recursion while still collapsing finite recursive", SoftBreak, Str "computations at compile time."], Header 2 ("zero-cost-abstractions", ["unnumbered", "unlisted"], []) [Str "Zero-cost abstractions"], Para [Str "Because the reducer runs before code generation, any abstraction that is fully", SoftBreak, Str "known at compile time disappears entirely from the output:"], CodeBlock ("", ["milang"], []) "-- Configuration record — reduced away at compile time
config = {width = 800; height = 600}
pixels = config.width * config.height
", CodeBlock ("", [""], []) "config =  {width = 800, height = 600}
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

- Type aliases such as `Int = Int' 64` are treated as syntactic sugar and are
  resolved during reduction; they do not remain at runtime.

- Current implementation note: sized types are primarily type-level and used
  for ABI/representation and annotation. The reducer evaluates constant
  arithmetic using Milang's general numeric semantics and does not automatically
  wrap or clamp values to a narrower machine width. If strict fixed-width
  arithmetic is required, use explicit conversion primitives or the C FFI,
  which maps milang sized types to fixed-width C types.

- Future work may include full width-aware reduction (clamping/wrapping at
  reduction time) and richer type-level computation.

-->"]