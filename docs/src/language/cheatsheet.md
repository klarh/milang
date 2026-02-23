# Milang Syntax Cheatsheet

Milang is a functional language with **zero keywords**, Haskell-like syntax, and
partial evaluation as the core compilation model. Everything is an expression.

## Literals

```text
42              -- integer
3.14            -- float
"hello"         -- string (supports \n \t \\ \")
"""
  multi-line    -- triple-quoted string (Swift-style margin stripping)
  string        -- closing """ indentation defines the margin
  """
[]              -- empty list (Nil record)
[1, 2, 3]      -- list literal (desugars to Cons/Nil chain)
```

## Bindings

```milang
x = 42                    -- value binding
f x y = x + y             -- function binding (params before =)
lazy := expensive_calc    -- lazy binding (thunk, evaluated on first use)
```

## Functions & Application

```milang
f x y = x + y             -- define: name params = body
f 3 4                     -- apply: juxtaposition (left-associative)
(\x -> x + 1)             -- lambda
(\x y -> x + y)           -- multi-param lambda
f 3 |> g                  -- pipe: g (f 3)
f >> g                    -- compose left-to-right: \x -> g (f x)
f << g                    -- compose right-to-left: \x -> f (g x)
```

## Operators

All operators are just functions. Standard arithmetic, comparison, logical:

```text
+ - * / % **              -- arithmetic (** is power)
== /= < > <= >=           -- comparison
&& ||                     -- logical (short-circuit)
not x                     -- logical negation (function, not operator)
+ ++                      -- string concat (+ or ++)
:                         -- cons (right-assoc): 1 : 2 : [] = [1, 2]
```

Operators as functions and functions as operators:

```milang
(+) 3 4                   -- operator in prefix: 7
3 `add` 4                 -- function in infix (backtick syntax)
```

## Records

```text
-- Anonymous record
point = {x = 3; y = 4}

-- Field access
point.x                       -- 3

-- Positional access (by declaration order)
point._0                      -- 3 (first field)

-- Record update
point2 = point:{x = 10}      -- {x = 10, y = 4}

-- Nested access
world.io.println              -- chained field access

-- Destructuring
{x; y} = point               -- binds x=3, y=4
{myX = x; myY = y} = point   -- binds myX=3, myY=4

-- Parsing gotcha
-- When passing a record literal directly as an argument you may need to parenthesize
-- the literal or bind it to a name to avoid parse ambiguity. For example:
--   -- may need parentheses
--   getField ({a = 1}) "a"
--   -- or bind first
--   r = {a = 1}
--   getField r "a"
```

## ADTs (Algebraic Data Types)

Uppercase bindings with braces declare tagged constructors:

```milang
Shape = {Circle radius; Rect width height}

-- Creates constructors:
c = Circle 5              -- {radius = 5} tagged "Circle"
r = Rect 3 4              -- {width = 3, height = 4} tagged "Rect"

-- Named fields also work:
Shape = {Circle {radius}; Rect {width; height}}
```

## Pattern Matching

The `->` operator introduces match alternatives:

```text
-- Inline alternatives (separated by ;)
f x = x -> 0 = "zero"; 1 = "one"; _ = "other"

-- Indented alternatives
f x = x ->
  0 = "zero"
  1 = "one"
  _ = "other"

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
```

## Scopes & Multi-line Bodies

Indented lines under a binding form a scope:

```milang
-- With explicit body expression (body = the expression after =)
compute x = result
  doubled = x * 2
  result = doubled + 1
-- Returns: value of `result` (15 when x=7)

-- Without body expression (scope returns implicit record)
makeVec x y =
  magnitude = sqrt (x**2 + y**2)
  normalized = {x = x / magnitude; y = y / magnitude}
-- Returns: {magnitude = 5.0, normalized = {x = 0.6, y = 0.8}}

-- Bare expressions in scopes evaluate for effect, not included in record
main world =
  world.io.println "hello"       -- effect: prints, result discarded
  world.io.println "world"       -- effect: prints, result discarded
```

Inline scopes use braces:

```milang
f x = result { doubled = x * 2; result = doubled + 1 }
```

## IO & the World

IO uses capability-based design. `main` receives `world`:

```milang,run
main world =
  world.io.println "hello"           -- print line
  world.io.print "no newline"        -- print without newline
  line = world.io.readLine           -- read line from stdin
  contents = world.io.readFile "f"   -- read file
  world.io.writeFile "f" "data"      -- write file
  world.io.appendFile "f" "more"     -- append to file
  exists = world.io.exists "f"       -- check file exists
  world.io.remove "f"               -- delete file
  result = world.process.exec "ls"   -- run shell command
  world.process.exit 1               -- exit with code
  args = world.argv                  -- command-line args (list)
  val = world.getEnv "PATH"          -- environment variable
  0

-- Pass restricted capabilities to helpers
greet io = io.println "hello"
main world = greet world.io          -- only give IO, not process/fs
```

`main`'s return value is the process exit code (int -> exit code, non-int -> 0).

## Imports

```text
-- Local file import (result is a record of all top-level bindings)
math = import "lib/math.mi"
x = math.square 5

-- URL import (cached locally)
lib = import "https://example.com/lib.mi"

-- URL import with sha256 pinning (required for reproducibility)
lib = import' "https://example.com/lib.mi" ({sha256 = "a1b2c3..."})

-- C header import (auto-parses function signatures)
m = import "/usr/include/math.h"
x = m.sin 1.0

-- C header with source file linking
lib = import' "mylib.h" ({src = "mylib.c"})

-- C header with extended options
lib = import' "mylib.h" ({
  sources = ["a.c", "b.c"]
  flags = "-O2"
  include = "include"
  pkg = "libpng"
})
```

Binding names are always lowercase. Uppercase names are reserved for type
declarations (union types, record constructors, type annotations).

Use `milang pin <file>` to auto-discover URL imports and add sha256 hashes.

## Annotation Domains

Milang has five binding domains, each with its own operator:

```milang
-- Value domain (=) — what it computes
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
add :? "Add two numbers"
add :? {summary = "Add two numbers"; params = {a = "First"; b = "Second"}}
add :? """
  Add two numbers together.
  Returns their sum.
  """

-- Parse domain (:!) — operator precedence/associativity
(+) :! {prec = 6; assoc = Left}

-- All domains can coexist on one binding:
distance :? "Euclidean distance"
distance :: Point : Point : Num
distance :~ pure
distance p1 p2 = sqrt ((p2.x - p1.x)**2 + (p2.y - p1.y)**2)
```

## Thunks & Laziness

```milang
~expr                   -- thunk: delays evaluation
x := expensive          -- lazy binding: creates thunk, evaluates once on use
```

## Metaprogramming

```milang
#expr                   -- quote: capture AST as a record
$expr                   -- splice: evaluate quoted AST back to code
f #param = $param       -- auto-quote param: compiler quotes arg at call site
```

## Comments

```text
-- line comment
/* block comment (nestable) */
```

## Built-in Functions

### Core
- `if cond then else` — conditional (auto-quotes branches via `#`-params)
- `len xs` — length of string or list
- `toString x` — convert to string
- `toInt s` — parse string to int; returns `Just` on success, `Nothing` on failure
- `toFloat s` — parse string to float; returns `Just` on success, `Nothing` on failure

### String
- `charAt i s` — character at index; returns `Just` character when index valid, otherwise `Nothing`
- `slice start end s` — substring
- `indexOf needle haystack` — find substring (-1 if not found)
- `split sep s` — split string by separator
- `trim s` — strip whitespace
- `toUpper s` / `toLower s` — case conversion
- `replace old new s` — string replacement

### List (prelude)
- `head xs` / `tail xs` / `last xs` / `init xs` — return `Maybe` (`Just` value or `Nothing`)
- `map f xs` / `filter f xs` / `fold f acc xs`
- `concat xs ys` / `push xs x` / `reverse xs`
- `take n xs` / `drop n xs` / `slice start end xs`
- `zip xs ys` / `enumerate xs` / `range start end`
- `sum xs` / `product xs` / `join sep xs`
- `any f xs` / `all f xs` / `contains x xs`
- `at lst i` / `at' i lst` — element at index (returns `Maybe`)
- `sort xs` / `sortBy f xs`

### Record introspection
- `fields r` — list of `{name, value}` records
- `fieldNames r` — list of field name strings
- `tag r` — constructor `tag` string (or "")
- `getField r name` — dynamic field access; returns `Just value` if present, otherwise `Nothing`.
- `setField r name val` — return new record with field set

### Utility
- `id x` / `const x y` / `flip f x y`
- `abs x` / `min a b` / `max a b`

## Compiler Modes

```bash
milang run file.mi          # compile + run
milang compile file.mi o.c  # emit C code
milang dump file.mi         # show reduced AST
milang repl                 # interactive REPL
```
