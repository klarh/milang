[Header 1 ("scopes--bindings", [], []) [Str "Scopes & Bindings"], Para [Str "Scopes are the backbone of milang's structure. Every indented block and every", SoftBreak, Str "brace-delimited block creates a new scope with its own bindings."], Header 2 ("basic-bindings", ["unnumbered", "unlisted"], []) [Str "Basic Bindings"], CodeBlock ("", ["milang"], []) "name = expr            -- eager binding
name := expr           -- lazy binding (thunk, evaluated at most once)
name params = expr     -- function binding
", CodeBlock ("", ["milang"], []) "x = 42
double x = x * 2
result = double x
", CodeBlock ("", [""], []) "x = 42
double = <closure>
result = 84
", Header 2 ("indentation-sensitive-scoping", ["unnumbered", "unlisted"], []) [Str "Indentation-Sensitive Scoping"], Para [Str "Indented lines beneath a binding form a scope. There are two modes depending", SoftBreak, Str "on whether an explicit result expression appears after ", Code ("", [], []) "=", Str "."], Header 3 ("explicit-body", ["unnumbered", "unlisted"], []) [Str "Explicit Body"], Para [Str "When a binding has an expression directly after ", Code ("", [], []) "=", Str ", that expression is the", SoftBreak, Str "scope's return value. The indented children are local definitions visible only", SoftBreak, Str "inside that scope:"], CodeBlock ("", ["milang"], []) "compute x = result
  doubled = x * 2
  result = doubled + 1
a = compute 7
", CodeBlock ("", [""], []) "compute = <closure>
a = 15
", Para [Str "Here ", Code ("", [], []) "doubled", Str " and ", Code ("", [], []) "result", Str " are local to ", Code ("", [], []) "compute", Str ". The value of ", Code ("", [], []) "compute 7", SoftBreak, Str "is the expression after ", Code ("", [], []) "=", Str ", which is ", Code ("", [], []) "result", Str " (15)."], Header 3 ("implicit-record-scope-as-record", ["unnumbered", "unlisted"], []) [Str "Implicit Record (Scope-as-Record)"], Para [Str "When a binding has ", Strong [Str "no"], Str " expression after ", Code ("", [], []) "=", Str " — only indented children — the", SoftBreak, Str "named bindings are collected into a record and returned automatically:"], CodeBlock ("", ["milang"], []) "makeVec x y =
  sum = x + y
  product = x * y
v = makeVec 3 4
", CodeBlock ("", [""], []) "makeVec = <closure>
v =  {sum = 7, product = 12}
", Para [Code ("", [], []) "makeVec 3 4", Str " returns ", Code ("", [], []) "{sum = 7, product = 12}", Str ". This is milang's lightweight", SoftBreak, Str "alternative to explicit record construction."], Header 2 ("inner-scopes-shadow-outer-names", ["unnumbered", "unlisted"], []) [Str "Inner Scopes Shadow Outer Names"], Para [Str "A binding in an inner scope shadows any identically-named binding from an", SoftBreak, Str "enclosing scope. The outer binding is unaffected:"], CodeBlock ("", ["milang"], []) "x = 10
f = result
  x = 99
  result = x + 1
outer = x
inner = f
", CodeBlock ("", [""], []) "x = 10
f = 100
outer = 10
inner = 100
", Header 2 ("inline-scopes-with-blocks", ["unnumbered", "unlisted"], []) [Str "Inline Scopes (With Blocks)"], Para [Str "Braces create an inline scope on a single line. The expression before the", SoftBreak, Str "braces is the return value, and the bindings inside are local:"], CodeBlock ("", ["milang"], []) "f x = result { doubled = x * 2; result = doubled + 1 }
a = f 7
", CodeBlock ("", [""], []) "f = <closure>
a = 15
", Header 2 ("bare-expressions-effect-statements", ["unnumbered", "unlisted"], []) [Str "Bare Expressions (Effect Statements)"], Para [Str "A bare expression in a scope — one not bound to a name — is evaluated for its", SoftBreak, Str "side effect. Its result is discarded and ", Strong [Str "not"], Str " included in any implicit", SoftBreak, Str "record:"], CodeBlock ("", ["milang"], []) "main world =
  world.io.println \"hello\"    -- effect, result discarded
  world.io.println \"world\"    -- effect, result discarded
  0                           -- explicit body (exit code)
", Para [Str "The first two lines run ", Code ("", [], []) "println", Str " for their side effects. The final ", Code ("", [], []) "0", Str " is the", SoftBreak, Str "return value of ", Code ("", [], []) "main", Str "."], Header 2 ("the-main-function-pattern", ["unnumbered", "unlisted"], []) [Str "The ", Code ("", [], []) "main", Str " Function Pattern"], Para [Str "A typical ", Code ("", [], []) "main", Str " combines all three concepts — local bindings, bare effect", SoftBreak, Str "expressions, and an explicit result:"], CodeBlock ("", ["milang"], []) "main world =
  name = \"milang\"                      -- local binding
  world.io.println (\"Hello, \" + name)  -- bare effect
  0                                    -- return value (exit code)
", Header 2 ("binding-order", ["unnumbered", "unlisted"], []) [Str "Binding Order"], BulletList [[Plain [Str "Bindings evaluate top-to-bottom (left-to-right in brace scopes)."]], [Plain [Str "Later bindings may reference earlier ones."]], [Plain [Str "The compiler tracks impure (", Code ("", [], []) "world", Str "-tainted) bindings and guarantees they", SoftBreak, Str "execute in declaration order via an auto-monad spine."]], [Plain [Str "Pure bindings can theoretically be reordered by the optimiser."]]]]