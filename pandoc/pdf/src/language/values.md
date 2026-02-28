[Header 1 ("values--literals", [], []) [Str "Values & Literals"], Para [Str "This chapter covers the literal forms you can write directly in source code."], Header 2 ("integers", ["unnumbered", "unlisted"], []) [Str "Integers"], Para [Str "Integer literals are written as decimal digits. Negative integers use a leading", SoftBreak, Str "minus sign attached to the literal. At compile time integers have arbitrary", SoftBreak, Str "precision; at runtime they default to ", Code ("", [], []) "int64_t", Str " (signed 64-bit)."], Para [Str "The type system supports sized integers via ", Code ("", [], []) "Int'", Str " (signed) and ", Code ("", [], []) "UInt'", SoftBreak, Str "(unsigned) type constructors that take a bit width: ", Code ("", [], []) "Int' 8", Str ", ", Code ("", [], []) "Int' 32", Str ",", SoftBreak, Code ("", [], []) "UInt' 64", Str ", etc. The prelude provides aliases: ", Code ("", [], []) "Int = Int' 64", Str ",", SoftBreak, Code ("", [], []) "UInt = UInt' 64", Str ", ", Code ("", [], []) "Byte = UInt' 8", Str "."], CodeBlock ("", ["milang"], []) "small = 42
zero = 0
negative = -3
big = 2 ** 32
", CodeBlock ("", [""], []) "small = 42
zero = 0
negative = -3
big = 4294967296
", Header 2 ("floats", ["unnumbered", "unlisted"], []) [Str "Floats"], Para [Str "Floating-point literals require digits on both sides of the decimal point.", SoftBreak, Str "They default to C ", Code ("", [], []) "double", Str " (64-bit). Negative floats use a leading minus sign.", SoftBreak, Str "Sized floats are available via ", Code ("", [], []) "Float'", Str ": ", Code ("", [], []) "Float' 32", Str " for single precision,", SoftBreak, Code ("", [], []) "Float' 64", Str " for double precision. The prelude alias ", Code ("", [], []) "Float = Float' 64", Str "."], CodeBlock ("", ["milang"], []) "pi = 3.14
half = 0.5
neg = -2.718
", CodeBlock ("", [""], []) "pi = 3.14
half = 0.5
", Header 2 ("strings", ["unnumbered", "unlisted"], []) [Str "Strings"], Para [Str "Strings are double-quoted and support the escape sequences ", Code ("", [], []) "\\n", Str ", ", Code ("", [], []) "\\t", Str ", ", Code ("", [], []) "\\\\", Str ",", SoftBreak, Str "and ", Code ("", [], []) "\\\"", Str "."], CodeBlock ("", ["milang"], []) "greeting = \"hello, world\"
escaped = \"line one\\nline two\"
length = len greeting
", CodeBlock ("", [""], []) "greeting = hello, world
escaped = line one
line two
length = 12
", Header 3 ("triple-quoted-strings", ["unnumbered", "unlisted"], []) [Str "Triple-Quoted Strings"], Para [Str "Triple-quoted strings span multiple lines with automatic margin stripping", SoftBreak, Str "(Swift-style). The indentation of the closing ", Code ("", [], []) "\"\"\"", Str " defines the margin —", SoftBreak, Str "everything to the left of that column is removed."], CodeBlock ("", ["milang"], []) "msg = \"\"\"
  Hello, world!
    indented line
  \"\"\"
", CodeBlock ("", [""], []) "msg = Hello, world!
  indented line
", Header 2 ("booleans", ["unnumbered", "unlisted"], []) [Str "Booleans"], Para [Str "There is no dedicated boolean type. Milang uses integers: ", Code ("", [], []) "1", Str " is true, ", Code ("", [], []) "0", Str " is", SoftBreak, Str "false. Comparison and logical operators return ", Code ("", [], []) "1", Str " or ", Code ("", [], []) "0", Str ", and ", Code ("", [], []) "if", Str " treats ", Code ("", [], []) "0", SoftBreak, Str "as false and any non-zero value as true."], CodeBlock ("", ["milang"], []) "yes = 1
no = 0
check = 3 > 2
", CodeBlock ("", [""], []) "yes = 1
no = 0
check = 1
", Header 2 ("lists", ["unnumbered", "unlisted"], []) [Str "Lists"], Para [Str "Lists are linked-list cons cells declared in the prelude as", SoftBreak, Code ("", [], []) "List = {Nil; Cons head tail}", Str ". The literal syntax ", Code ("", [], []) "[1, 2, 3]", Str " desugars into", SoftBreak, Str "a chain of ", Code ("", [], []) "Cons", Str "/", Code ("", [], []) "Nil", Str " records. The cons operator ", Code ("", [], []) ":", Str " is right-associative."], CodeBlock ("", ["milang"], []) "nums = [1, 2, 3]
empty = []
consed = 10 : 20 : 30 : []
", CodeBlock ("", [""], []) "nums = [1, 2, 3]
empty = []
consed = [10, 20, 30]
", Para [Str "See the ", Link ("", [], []) [Str "Lists"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/pdf/src/language/lists.md#lists", ""), Str " chapter for the full prelude API."], Header 2 ("records", ["unnumbered", "unlisted"], []) [Str "Records"], Para [Str "A record is a set of named ", Code ("", [], []) "fields", Str " enclosed in braces and separated by ", Code ("", [], []) ";", Str " or", SoftBreak, Str "newlines."], CodeBlock ("", ["milang"], []) "point = {x = 3; y = 4}
access = point.x + point.y
", CodeBlock ("", [""], []) "point =  {x = 3, y = 4}
access = 7
", Para [Str "See the ", Link ("", [], []) [Str "Records & ADTs"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/pdf/src/language/records.md#records--adts", ""), Str " chapter for updates, destructuring, and", SoftBreak, Str "algebraic data types."], Header 2 ("constructors", ["unnumbered", "unlisted"], []) [Str "Constructors"], Para [Str "An uppercase name applied to arguments creates a tagged record. Tags are", SoftBreak, Str "introduced by ADT declarations or used ad-hoc."], CodeBlock ("", ["milang"], []) "Shape = {Circle radius; Rect width height}
c = Circle 5
r = Rect 3 4
", CodeBlock ("", [""], []) "Shape = _module_ {Circle = <closure>, Rect = <closure>}
Circle = <closure>
Rect = <closure>
c = Circle {radius = 5}
r = Rect {width = 3, height = 4}
", Header 2 ("functions-as-values", ["unnumbered", "unlisted"], []) [Str "Functions as Values"], Para [Str "Functions are first-class values. They can be bound to names, passed as", SoftBreak, Str "arguments, and returned from other functions. A function that has not received", SoftBreak, Str "all of its arguments displays as ", Code ("", [], []) "<closure>", Str "."], CodeBlock ("", ["milang"], []) "add x y = x + y
inc = add 1
result = inc 10
", CodeBlock ("", [""], []) "add = <closure>
inc = <closure>
result = 11
", Para [Str "See the ", Link ("", [], []) [Str "Functions"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/pdf/src/language/functions.md#functions", ""), Str " chapter for lambdas, pipes, and more."]]