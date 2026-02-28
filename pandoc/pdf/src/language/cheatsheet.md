[Header 1 ("milang-syntax-cheatsheet", [], []) [Str "Milang Syntax Cheatsheet"], Para [Str "Milang is a functional language with ", Strong [Str "zero keywords"], Str ", Haskell-like syntax, and", SoftBreak, Str "partial evaluation as the core compilation model. Everything is an expression."], Header 2 ("literals", ["unnumbered", "unlisted"], []) [Str "Literals"], CodeBlock ("", ["text"], []) "42              -- integer
3.14            -- float
\"hello\"         -- string (supports \\n \\t \\\\ \\\")
\"\"\"
  multi-line    -- triple-quoted string (Swift-style margin stripping)
  string        -- closing \"\"\" indentation defines the margin
  \"\"\"
[]              -- empty list (Nil record)
[1, 2, 3]      -- list literal (desugars to Cons/Nil chain)
", Header 2 ("bindings", ["unnumbered", "unlisted"], []) [Str "Bindings"], CodeBlock ("", ["milang"], []) "x = 42                    -- value binding
f x y = x + y             -- function binding (params before =)
lazy := expensive_calc    -- lazy binding (thunk, evaluated on first use)
", Header 2 ("functions--application", ["unnumbered", "unlisted"], []) [Str "Functions & Application"], CodeBlock ("", ["milang"], []) "f x y = x + y             -- define: name params = body
f 3 4                     -- apply: juxtaposition (left-associative)
(\\x -> x + 1)             -- lambda
(\\x y -> x + y)           -- multi-param lambda
f 3 |> g                  -- pipe: g (f 3)
f >> g                    -- compose left-to-right: \\x -> g (f x)
f << g                    -- compose right-to-left: \\x -> f (g x)
", Header 2 ("operators", ["unnumbered", "unlisted"], []) [Str "Operators"], Para [Str "All operators are just functions. Standard arithmetic, comparison, logical:"], CodeBlock ("", ["text"], []) "+ - * / % **              -- arithmetic (** is power)
== /= < > <= >=           -- comparison
&& ||                     -- logical (short-circuit)
not x                     -- logical negation (function, not operator)
+ `+`                       -- string concat (use `+` for both numeric and string)
:                         -- cons (right-assoc): 1 : 2 : [] = [1, 2]
", Para [Str "Operators as functions and functions as operators:"], CodeBlock ("", ["milang"], []) "(+) 3 4                   -- operator in prefix: 7
3 `add` 4                 -- function in infix (backtick syntax)
", Header 2 ("records", ["unnumbered", "unlisted"], []) [Str "Records"], CodeBlock ("", ["text"], []) "-- Anonymous record
point = {x = 3; y = 4}

-- Field access
point.x                       -- 3

-- Positional access (by declaration order)
point._0                      -- 3 (first field)

-- Record update
point2 = point <- {x = 10}      -- {x = 10, y = 4}

-- Nested access
world.io.println              -- chained field access

-- Destructuring
{x; y} = point               -- binds x=3, y=4
{myX = x; myY = y} = point   -- binds myX=3, myY=4

-- Parsing gotcha
-- When passing a record literal directly as an argument you may need to parenthesize
-- the literal or bind it to a name to avoid parse ambiguity. For example:
--   -- may need parentheses
--   getField ({a = 1}) \"a\"
--   -- or bind first
--   r = {a = 1}
--   getField r \"a\"
", Header 2 ("adts-algebraic-data-types", ["unnumbered", "unlisted"], []) [Str "ADTs (Algebraic Data Types)"], Para [Str "Uppercase bindings with braces declare tagged constructors:"], CodeBlock ("", ["milang"], []) "Shape = {Circle radius; Rect width height}

-- Creates constructors:
c = Circle 5              -- {radius = 5} tagged \"Circle\"
r = Rect 3 4              -- {width = 3, height = 4} tagged \"Rect\"

-- Named fields also work:
Shape = {Circle {radius}; Rect {width; height}}
", Header 2 ("pattern-matching", ["unnumbered", "unlisted"], []) [Str "Pattern Matching"], Para [Str "The ", Code ("", [], []) "->", Str " operator introduces match alternatives:"], CodeBlock ("", ["text"], []) "-- Inline alternatives (separated by ;)
f x = x -> 0 = \"zero\"; 1 = \"one\"; _ = \"other\"

-- Indented alternatives
f x = x ->
  0 = \"zero\"
  1 = \"one\"
  _ = \"other\"

-- Pattern types
42                        -- literal match
x                         -- variable (binds anything)
_                         -- wildcard (match, don't bind)
Circle                    -- constructor tag match
Rect                      -- constructor tag match (fields accessible via .field)
[a, b, c]                 -- list pattern (exact length)
[first, ...rest]          -- list pattern with spread

-- Guards (| condition = body)
abs x = x ->
  n | n >= 0 = n
  n = 0 - n

-- Pattern matching in case expressions
area s = s ->
  Circle = 3.14 * s.radius * s.radius
  Rect = s.width * s.height
", Header 2 ("scopes--multi-line-bodies", ["unnumbered", "unlisted"], []) [Str "Scopes & Multi-line Bodies"], Para [Str "Indented lines under a binding form a scope:"], CodeBlock ("", ["milang"], []) "-- With explicit body expression (body = the expression after =)
compute x = result
  doubled = x * 2
  result = doubled + 1
-- Returns: value of `result` (15 when x=7)

-- Without body expression (scope returns implicit record)
makeVec x y =
  dx = x ** 2
  dy = y ** 2
  sumSquares = dx + dy
-- Returns: {dx = 49, dy = 9, sumSquares = 58}

-- Bare expressions in scopes evaluate for effect, not included in record
main world =
  world.io.println \"hello\"       -- effect: prints, result discarded
  world.io.println \"world\"       -- effect: prints, result discarded
", Para [Str "Inline scopes use braces:"], CodeBlock ("", ["milang"], []) "f x = result { doubled = x * 2; result = doubled + 1 }
", Header 2 ("io--the-world", ["unnumbered", "unlisted"], []) [Str "IO & the World"], Para [Str "IO uses capability-based design. ", Code ("", [], []) "main", Str " receives ", Code ("", [], []) "world", Str ":"], CodeBlock ("", ["milang"], []) "main world =
  world.io.println \"hello\"              -- print line
  world.io.print \"no newline\"           -- print without newline
  line = world.io.readLine              -- read line from stdin
  contents = world.fs.read.file \"f\"     -- read file
  world.fs.write.file \"f\" \"data\"        -- write file
  world.fs.write.append \"f\" \"more\"      -- append to file
  exists = world.fs.read.exists \"f\"     -- check file exists
  world.fs.write.remove \"f\"             -- delete file
  result = world.process.exec \"ls\"      -- run shell command
  world.process.exit 1                  -- exit with code
  args = world.argv                     -- command-line args (list)
  val = world.getEnv \"PATH\"             -- environment variable
  0

-- Pass restricted capabilities to helpers
greet io = io.println \"hello\"
main world = greet world.io          -- only give IO, not process/fs
", CodeBlock ("", [""], []) "hello
", Para [Code ("", [], []) "main", Str "'s return value is the process exit code (int -> exit code, non-int -> 0)."], Header 2 ("imports", ["unnumbered", "unlisted"], []) [Str "Imports"], CodeBlock ("", ["text"], []) "-- Local file import (result is a record of all top-level bindings)
math = import \"lib/math.mi\"
x = math.square 5

-- URL import (cached locally)
lib = import \"https://example.com/lib.mi\"

-- URL import with sha256 pinning (required for reproducibility)
lib = import' \"https://example.com/lib.mi\" ({sha256 = \"a1b2c3...\"})

-- C header import (auto-parses function signatures)
m = import \"/usr/include/math.h\"
x = m.sin 1.0

-- C header with source file linking
lib = import' \"mylib.h\" ({src = \"mylib.c\"})

-- C header with extended options
lib = import' \"mylib.h\" ({
  sources = [\"a.c\", \"b.c\"]
  flags = \"-O2\"
  include = \"include\"
  pkg = \"libpng\"
})
", Para [Str "Binding names are always lowercase. Uppercase names are reserved for type", SoftBreak, Str "declarations (union types, record constructors, type annotations)."], Para [Str "Use ", Code ("", [], []) "milang pin <file>", Str " to auto-discover URL imports and add sha256 hashes."], Header 2 ("annotation-domains", ["unnumbered", "unlisted"], []) [Str "Annotation Domains"], Para [Str "Milang has five binding domains, each with its own operator:"], CodeBlock ("", ["milang"], []) "-- Value domain (=) — what it computes
add a b = a + b

-- Type domain (::) — structural type annotation
add :: Num : Num : Num

-- Sized numeric types: Int', UInt', Float' take a bit width
-- Prelude aliases: Int = Int' 64, UInt = UInt' 64, Float = Float' 64, Byte = UInt' 8
add8 :: Int' 8 : Int' 8 : Int' 8

-- Traits domain (:~) — computational attributes / effect sets
add :~ pure                          -- pure = [] (no effects)
greet :~ [console]                   -- needs console capability
server :~ [console, fs.read, fs.write]

-- Documentation domain (:?) — human-readable docs
add :? \"Add two numbers\"
add :? {summary = \"Add two numbers\"; params = {a = \"First\"; b = \"Second\"}}
add :? \"\"\"
  Add two numbers together.
  Returns their sum.
  \"\"\"

-- Parse domain (:!) — operator precedence/associativity
(+) :! {prec = 6; assoc = Left}

-- All domains can coexist on one binding:
distance :? \"Euclidean distance\"
distance :: Point : Point : Num
distance :~ pure
distance p1 p2 = (p2.x - p1.x)**2 + (p2.y - p1.y)**2
", Header 2 ("thunks--laziness", ["unnumbered", "unlisted"], []) [Str "Thunks & Laziness"], CodeBlock ("", ["milang"], []) "~expr                   -- thunk: delays evaluation
x := expensive          -- lazy binding: creates thunk, evaluates once on use
", Header 2 ("metaprogramming", ["unnumbered", "unlisted"], []) [Str "Metaprogramming"], CodeBlock ("", ["milang"], []) "#expr                   -- quote: capture AST as a record
$expr                   -- splice: evaluate quoted AST back to code
f #param = $param       -- auto-quote param: compiler quotes arg at call site
", Header 2 ("comments", ["unnumbered", "unlisted"], []) [Str "Comments"], CodeBlock ("", ["text"], []) "-- line comment
/* block comment (nestable) */
", Header 2 ("built-in-functions", ["unnumbered", "unlisted"], []) [Str "Built-in Functions"], Header 3 ("core", ["unnumbered", "unlisted"], []) [Str "Core"], BulletList [[Plain [Code ("", [], []) "if cond then else", Str " — conditional (auto-quotes branches via ", Code ("", [], []) "#", Str "-params)"]], [Plain [Code ("", [], []) "len xs", Str " — length of string or list"]], [Plain [Code ("", [], []) "toString x", Str " — convert to string"]], [Plain [Code ("", [], []) "toInt s", Str " — parse string to int; returns ", Code ("", [], []) "Just", Str " on success, ", Code ("", [], []) "Nothing", Str " on failure"]], [Plain [Code ("", [], []) "toFloat s", Str " — parse string to float; returns ", Code ("", [], []) "Just", Str " on success, ", Code ("", [], []) "Nothing", Str " on failure"]]], Header 3 ("string", ["unnumbered", "unlisted"], []) [Str "String"], BulletList [[Plain [Code ("", [], []) "charAt i s", Str " — character at index; returns ", Code ("", [], []) "Just", Str " character when index valid, otherwise ", Code ("", [], []) "Nothing"]], [Plain [Code ("", [], []) "slice start end s", Str " — substring"]], [Plain [Code ("", [], []) "indexOf needle haystack", Str " — find substring (-1 if not found)"]], [Plain [Code ("", [], []) "split sep s", Str " — split string by separator"]], [Plain [Code ("", [], []) "trim s", Str " — strip whitespace"]], [Plain [Code ("", [], []) "toUpper s", Str " / ", Code ("", [], []) "toLower s", Str " — case conversion"]], [Plain [Code ("", [], []) "replace old new s", Str " — string replacement"]]], Header 3 ("list-prelude", ["unnumbered", "unlisted"], []) [Str "List (prelude)"], BulletList [[Plain [Code ("", [], []) "head xs", Str " / ", Code ("", [], []) "tail xs", Str " / ", Code ("", [], []) "last xs", Str " / ", Code ("", [], []) "init xs", Str " — return ", Code ("", [], []) "Maybe", Str " (", Code ("", [], []) "Just", Str " value or ", Code ("", [], []) "Nothing", Str ")"]], [Plain [Code ("", [], []) "map f xs", Str " / ", Code ("", [], []) "filter f xs", Str " / ", Code ("", [], []) "fold f acc xs"]], [Plain [Code ("", [], []) "concat xs ys", Str " / ", Code ("", [], []) "push xs x", Str " / ", Code ("", [], []) "reverse xs"]], [Plain [Code ("", [], []) "take n xs", Str " / ", Code ("", [], []) "drop n xs", Str " / ", Code ("", [], []) "slice start end xs"]], [Plain [Code ("", [], []) "zip xs ys", Str " / ", Code ("", [], []) "enumerate xs", Str " / ", Code ("", [], []) "range start end"]], [Plain [Code ("", [], []) "sum xs", Str " / ", Code ("", [], []) "product xs", Str " / ", Code ("", [], []) "join sep xs"]], [Plain [Code ("", [], []) "any f xs", Str " / ", Code ("", [], []) "all f xs", Str " / ", Code ("", [], []) "contains x xs"]], [Plain [Code ("", [], []) "at lst i", Str " / ", Code ("", [], []) "at' i lst", Str " — element at index (returns ", Code ("", [], []) "Maybe", Str ")"]], [Plain [Code ("", [], []) "sort xs", Str " / ", Code ("", [], []) "sortBy f xs"]]], Header 3 ("record-introspection", ["unnumbered", "unlisted"], []) [Str "Record introspection"], BulletList [[Plain [Code ("", [], []) "fields r", Str " — list of ", Code ("", [], []) "{name, value}", Str " records"]], [Plain [Code ("", [], []) "fieldNames r", Str " — list of field name strings"]], [Plain [Code ("", [], []) "tag r", Str " — constructor ", Code ("", [], []) "tag", Str " string (or \"\")"]], [Plain [Code ("", [], []) "getField r name", Str " — dynamic field access; returns ", Code ("", [], []) "Just value", Str " if present, otherwise ", Code ("", [], []) "Nothing", Str "."]], [Plain [Code ("", [], []) "setField r name val", Str " — return new record with field set"]]], Header 3 ("utility", ["unnumbered", "unlisted"], []) [Str "Utility"], BulletList [[Plain [Code ("", [], []) "id x", Str " / ", Code ("", [], []) "const x y", Str " / ", Code ("", [], []) "flip f x y"]], [Plain [Code ("", [], []) "abs x", Str " / ", Code ("", [], []) "min a b", Str " / ", Code ("", [], []) "max a b"]]], Header 2 ("compiler-modes", ["unnumbered", "unlisted"], []) [Str "Compiler Modes"], CodeBlock ("", ["bash"], []) "milang run file.mi          # compile + run
milang compile file.mi o.c  # emit C code
milang dump file.mi         # show parsed AST
milang reduce file.mi       # show partially-evaluated AST
milang repl                 # interactive REPL
"]