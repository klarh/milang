[Header 1 ("common-pitfalls", [], []) [Str "Common Pitfalls"], Para [Str "A guide to the most frequent mistakes and surprises in milang,", SoftBreak, Str "especially for developers coming from Haskell, Python, or other", SoftBreak, Str "mainstream languages."], Header 2 ("record-literals-must-be-parenthesized-as-arguments", ["unnumbered", "unlisted"], []) [Str "Record Literals Must Be Parenthesized as Arguments"], Para [Str "A bare ", Code ("", [], []) "{...}", Str " after an expression is parsed as a ", Strong [Str "record update"], Str " (", Code ("", [], []) "<-", Str "),", SoftBreak, Str "not a function argument."], CodeBlock ("", ["milang"], []) "-- WRONG: parsed as (if cond) <- {x = 1} — a record update on if's result
f cond = if cond {x = 1} {x = 2}

-- RIGHT: parenthesize the record literal
f cond = if cond ({x = 1}) ({x = 2})

-- RIGHT: bind first, then pass
f cond = if cond a b
  a = {x = 1}
  b = {x = 2}
", Para [Str "When a record literal follows a function call without parentheses, the", SoftBreak, Str "parser treats it as a record update expression instead of a function", SoftBreak, Str "argument."], Header 2 ("blocks-return-records-not-the-last-expression", ["unnumbered", "unlisted"], []) [Str "Blocks Return Records, Not the Last Expression"], Para [Str "Unlike Python or Rust, a block of bindings returns the ", Strong [Str "record of all", SoftBreak, Str "named bindings"], Str ", not the last expression."], CodeBlock ("", ["milang"], []) "-- Returns {doubled = 14, tripled = 21} — NOT tripled
compute x =
  doubled = x * 2
  tripled = x * 3

-- To return a specific value, put the body expression FIRST:
compute x = tripled
  doubled = x * 2
  tripled = x * 3
", Para [Str "The with-binding pattern — body expression on the same line as ", Code ("", [], []) "=", Str ",", SoftBreak, Str "local bindings indented below — is the standard way to write functions", SoftBreak, Str "that return a specific value."], Header 2 ("line-continuation-with-", ["unnumbered", "unlisted"], []) [Str "Line Continuation with ", Code ("", [], []) "\\"], Para [Str "Long expressions can be split across lines using ", Code ("", [], []) "\\", Str " at the end of a line:"], CodeBlock ("", ["milang"], []) "result = very_long_function_name arg1 \\
  arg2 arg3
", Para [Str "The ", Code ("", [], []) "\\", Str " joins the next line to the current one, ignoring the newline", SoftBreak, Str "and indentation."], Header 2 ("if-is-a-function-not-a-keyword", ["unnumbered", "unlisted"], []) [Code ("", [], []) "if", Str " Is a Function, Not a Keyword"], Para [Str "There are no keywords in milang.  ", Code ("", [], []) "if", Str " is a prelude function with", SoftBreak, Str "auto-quoted parameters (", Code ("", [], []) "#", Str "):"], CodeBlock ("", ["milang"], []) "if cond #t #e = (truthy cond) -> 0 = $e; _ = $t
", Para [Str "This means:"], BulletList [[Plain [Str "Both branches must be expressions (no statements or blocks)"]], [Plain [Str "Both branches are provided as arguments: ", Code ("", [], []) "if condition trueVal falseVal"]], [Plain [Str "There is no ", Code ("", [], []) "then", Str "/", Code ("", [], []) "else", Str " keyword"]]], CodeBlock ("", ["milang"], []) "-- Milang:
max a b = if (a > b) a b

-- Haskell equivalent:
-- max a b = if a > b then a else b

-- Python equivalent:
-- def max(a, b): return a if a > b else b
", Para [Str "For multi-line if expressions, keep the entire ", Code ("", [], []) "if", Str " on one line or use", SoftBreak, Str "with-bindings:"], CodeBlock ("", ["milang"], []) "-- WRONG: else branch at same indent as if → parsed as separate statement
f x =
  if (x > 0) \"positive\"
  \"non-positive\"

-- RIGHT: one line
f x = if (x > 0) \"positive\" \"non-positive\"

-- RIGHT: with-bindings
f x = result
  result = if (x > 0) \"positive\" \"non-positive\"
", Header 2 ("truthiness-integers-not-booleans", ["unnumbered", "unlisted"], []) [Str "Truthiness: Integers, Not Booleans"], Para [Str "Milang uses integers for truth values.  ", Code ("", [], []) "1", Str " is true, ", Code ("", [], []) "0", Str " is false.", SoftBreak, Code ("", [], []) "True", Str " and ", Code ("", [], []) "False", Str " are constructors (tagged records), but comparisons", SoftBreak, Str "and ", Code ("", [], []) "if", Str " work with integers:"], CodeBlock ("", ["milang"], []) "-- Comparisons return 0 or 1
3 > 2          -- 1
3 == 3         -- 1
\"a\" == \"b\"     -- 0

-- Truthy: anything non-zero, non-empty, non-Nothing, non-Nil
-- Falsy: 0, \"\", False, Nil, Nothing
if 42 \"yes\" \"no\"       -- \"yes\"
if \"\" \"yes\" \"no\"       -- \"no\"
if Nothing \"yes\" \"no\"  -- \"no\"

-- Convert any value to 0/1 with `truthy`:
truthy 42        -- 1
truthy \"\"        -- 0
truthy [1, 2]    -- 1
truthy Nothing   -- 0
", Header 2 ("not-equal-is--not-", ["unnumbered", "unlisted"], []) [Str "Not-Equal Is ", Code ("", [], []) "/=", Str ", Not ", Code ("", [], []) "!="], CodeBlock ("", ["milang"], []) "-- WRONG: != is not defined
x != y

-- RIGHT: use /= (same as Haskell)
x /= y

-- Also works: negate ==
not (x == y)
", Header 2 ("string-concatenation-uses-", ["unnumbered", "unlisted"], []) [Str "String Concatenation Uses ", Code ("", [], []) "+"], Para [Str "Unlike Haskell (", Code ("", [], []) "++", Str ") or Python (implicit), milang uses ", Code ("", [], []) "+", Str " for both", SoftBreak, Str "numeric addition and string concatenation:"], CodeBlock ("", ["milang"], []) "\"hello\" + \" \" + \"world\"   -- \"hello world\"
3 + 4                       -- 7
", Header 2 ("no-mutable-variables-or-loops", ["unnumbered", "unlisted"], []) [Str "No Mutable Variables or Loops"], Para [Str "There are no ", Code ("", [], []) "for", Str ", ", Code ("", [], []) "while", Str ", or assignment statements.  Use recursion", SoftBreak, Str "or higher-order prelude functions:"], CodeBlock ("", ["milang"], []) "-- Python:  sum = 0; for x in range(10): sum += x
-- Milang:
total = fold (\\acc x -> acc + x) 0 (range 0 10)

-- Python:  result = [x*2 for x in items]
-- Milang:
result = map (\\x -> x * 2) items

-- Python:  evens = [x for x in items if x % 2 == 0]
-- Milang:
evens = filter (\\x -> x % 2 == 0) items
", Para [Str "For iterative algorithms, use tail-recursive functions with the", SoftBreak, Str "with-binding pattern:"], CodeBlock ("", ["milang"], []) "-- Iterate until condition met
loop n = if (n >= 1000) n (loop next)
  next = n + 1
", Header 2 ("lists-are-linked-lists", ["unnumbered", "unlisted"], []) [Str "Lists Are Linked Lists"], Para [Str "Milang lists are Cons/Nil linked lists, not arrays:"], CodeBlock ("", ["milang"], []) "[1, 2, 3]    -- desugars to Cons 1 (Cons 2 (Cons 3 Nil))
", Para [Str "This means:"], BulletList [[Plain [Code ("", [], []) "head", Str ", ", Code ("", [], []) "tail", Str ", cons (", Code ("", [], []) ":", Str ") are O(1) — access/modify the front"]], [Plain [Code ("", [], []) "len", Str " is O(n) — must traverse the entire list to count elements"]], [Plain [Code ("", [], []) "at i lst", Str " is O(i) — must walk i links to reach element i"]], [Plain [Code ("", [], []) "last", Str " is O(n) — must walk to the final element"]], [Plain [Code ("", [], []) "push", Str " (append to end) is O(n) — must copy the entire spine"]], [Plain [Str "Prepend with ", Code ("", [], []) ":", Str " for O(1): ", Code ("", [], []) "x : xs"]]], CodeBlock ("", ["milang"], []) "-- SLOW: O(n²) — appends to end each iteration
build lst = fold (\\acc x -> push acc x) [] items

-- FAST: O(n) — prepend then reverse
build lst = reverse (fold (\\acc x -> x : acc) [] items)
", Header 2 ("imports-return-records", ["unnumbered", "unlisted"], []) [Str "Imports Return Records"], Para [Code ("", [], []) "import", Str " returns a record of all top-level bindings from the imported", SoftBreak, Str "file.  There is no automatic namespace injection:"], CodeBlock ("", ["milang"], []) "-- Python:  from math import sqrt
-- Haskell: import Math (sqrt)
-- Milang:
math = import \"math.mi\"
result = math.sqrt 16

-- To use names directly, destructure with {...}:
{sqrt; sin; cos} = import \"math.mi\"
result = sqrt 16
", Header 2 ("pattern-matching-uses---not-case--of", ["unnumbered", "unlisted"], []) [Str "Pattern Matching Uses ", Code ("", [], []) "->", Str ", Not ", Code ("", [], []) "case ... of"], CodeBlock ("", ["milang"], []) "-- Haskell:
-- case x of
--   0 -> \"zero\"
--   n -> \"other\"

-- Milang:
x -> 0 = \"zero\"; n = \"other\"

-- Or indented:
x ->
  0 = \"zero\"
  n = \"other\"
", Para [Str "Patterns use ", Code ("", [], []) "=", Str " to separate pattern from body (not ", Code ("", [], []) "->", Str " as in Haskell)."], Header 2 ("maybe-everywhere", ["unnumbered", "unlisted"], []) [Code ("", [], []) "Maybe", Str " Everywhere"], Para [Str "Functions that might fail return ", Code ("", [], []) "Maybe", Str " (", Code ("", [], []) "Just val", Str " or ", Code ("", [], []) "Nothing", Str "),", SoftBreak, Str "not exceptions or sentinel values:"], CodeBlock ("", ["milang"], []) "head []           -- Nothing
head [1, 2, 3]    -- Just 1
at [10, 20] 5     -- Nothing (out of bounds)
toInt \"abc\"       -- Nothing
toInt \"42\"        -- Just 42

-- Unwrap with pattern matching:
result = toInt input ->
  Just n = n * 2
  Nothing = 0

-- Or use default:
n = default 0 (toInt input)
", Header 2 ("semicolons-are-field-separators-not-statement-terminators", ["unnumbered", "unlisted"], []) [Str "Semicolons Are Field Separators, Not Statement Terminators"], Para [Str "In records and inline scopes, ", Code ("", [], []) ";", Str " separates fields.  It is not a", SoftBreak, Str "statement terminator:"], CodeBlock ("", ["milang"], []) "point = {x = 1; y = 2}           -- record with two fields
f x = x -> 0 = \"a\"; _ = \"b\"     -- pattern match with two alternatives
{a = 1; b = 2; c = 3}            -- three-field record
", Header 2 ("no-parentheses-around-function-arguments", ["unnumbered", "unlisted"], []) [Str "No Parentheses Around Function Arguments"], Para [Str "Unlike Python/C, function application is by juxtaposition.  Only use", SoftBreak, Str "parentheses to group sub-expressions:"], CodeBlock ("", ["milang"], []) "-- WRONG (Python habit): extra parens are harmless but unnecessary
f(x)              -- works but looks odd
f(x, y)           -- WRONG: this is f applied to a tuple-like record

-- RIGHT: Haskell-style juxtaposition
f x               -- apply f to x
f x y             -- apply f to x, then apply result to y
f (x + 1) y       -- parens only to group the sub-expression x + 1
", Header 2 ("pipe-operator-for-readability", ["unnumbered", "unlisted"], []) [Str "Pipe Operator for Readability"], Para [Str "Use ", Code ("", [], []) "|>", Str " to avoid deeply nested function calls:"], CodeBlock ("", ["milang"], []) "-- Nested (hard to read):
toString (sum (map (\\x -> x * 2) (filter (\\x -> x > 0) lst)))

-- Piped (reads left to right):
lst
  |> filter (\\x -> x > 0)
  |> map (\\x -> x * 2)
  |> sum
  |> toString
", Header 2 ("lazy-bindings-need-", ["unnumbered", "unlisted"], []) [Str "Lazy Bindings Need ", Code ("", [], []) ":="], Para [Str "A regular binding (", Code ("", [], []) "=", Str ") evaluates eagerly.  Use ", Code ("", [], []) ":=", Str " for lazy evaluation", SoftBreak, Str "(thunked — computed on first access):"], CodeBlock ("", ["milang"], []) "-- Evaluated immediately, even if never used
expensive = computeHeavyThing x

-- Evaluated only when first accessed
expensive := computeHeavyThing x
", Para [Str "Prefix ", Code ("", [], []) "~", Str " creates an inline thunk:"], CodeBlock ("", ["milang"], []) "pair = {now = compute x; later = ~(compute y)}
"]