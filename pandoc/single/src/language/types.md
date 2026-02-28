[Header 1 ("type-annotations-", [], []) [Str "Type Annotations (", Code ("", [], []) "::", Str ")"], Para [Str "Type annotations in milang are optional — the compiler infers types. When you do annotate, you use the ", Code ("", [], []) "::", Str " domain to attach a type expression to a binding. Annotations are separate lines that merge with the corresponding value binding."], Header 2 ("syntax", ["unnumbered", "unlisted"], []) [Str "Syntax"], CodeBlock ("", ["milang"], []) "name :: typeExpr
name args = body
", Para [Str "Inside a type expression, ", Code ("", [], []) ":", Str " means \"function type\" and is right-associative. So ", Code ("", [], []) "Num : Num : Num", Str " means \"a function that takes a ", Code ("", [], []) "Num", Str ", then a ", Code ("", [], []) "Num", Str ", and returns a ", Code ("", [], []) "Num", Str ".\""], Header 2 ("primitive-types", ["unnumbered", "unlisted"], []) [Str "Primitive Types"], Table ("", [], []) (Caption Nothing []) [(AlignDefault, ColWidthDefault), (AlignDefault, ColWidthDefault)] (TableHead ("", [], []) [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Type"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Description"]]]]) [(TableBody ("", [], []) (RowHeadColumns 0) [] [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "Num"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Alias for ", Code ("", [], []) "Int", Str " (backward compatibility)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "Int"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Signed 64-bit integer (alias for ", Code ("", [], []) "Int' 64", Str ")"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "UInt"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Unsigned 64-bit integer (alias for ", Code ("", [], []) "UInt' 64", Str ")"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "Float"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "64-bit floating-point (alias for ", Code ("", [], []) "Float' 64", Str ")"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "Byte"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Unsigned 8-bit integer (alias for ", Code ("", [], []) "UInt' 8", Str ")"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "Str"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "String"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "List"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Linked list (Cons/", Code ("", [], []) "Nil", Str ")"]]]])] (TableFoot ("", [], []) []), Header 3 ("sized-numeric-types", ["unnumbered", "unlisted"], []) [Str "Sized Numeric Types"], Para [Str "The primitive type constructors ", Code ("", [], []) "Int'", Str ", ", Code ("", [], []) "UInt'", Str ", and ", Code ("", [], []) "Float'", Str " take a compile-time bit width:"], CodeBlock ("", ["milang"], []) "add8 :: Int' 8 : Int' 8 : Int' 8
add8 a b = a + b

compact :: Float' 32 : Float' 32
compact x = x * 1.0
", Para [Str "The prelude defines convenient aliases:"], CodeBlock ("", ["milang"], []) "Int = Int' 64
UInt = UInt' 64
Float = Float' 64
Byte = UInt' 8
", Para [Str "You can define your own aliases:"], CodeBlock ("", ["milang"], []) "Short = Int' 16
Word = UInt' 32
", Header 3 ("details-on-sized-numeric-types", ["unnumbered", "unlisted"], []) [Str "Details on Sized Numeric Types"], Para [Str "The primitive constructors ", Code ("", [], []) "Int'", Str ", ", Code ("", [], []) "UInt'", Str ", and ", Code ("", [], []) "Float'", Str " take a compile-time", SoftBreak, Str "bit-width argument and provide fixed-width numeric types. The language treats", SoftBreak, Str "these as distinct primitive types rather than mere annotations:"], BulletList [[Para [Str "Signed integers (", Code ("", [], []) "Int' n", Str ") use two's-complement semantics; arithmetic on", SoftBreak, Str "signed integers is performed modulo 2^n and results are interpreted in two's", SoftBreak, Str "complement when read as signed values. Overflow wraps around (modular", SoftBreak, Str "arithmetic)."]], [Para [Str "Unsigned integers (", Code ("", [], []) "UInt' n", Str ") are arithmetic modulo 2^n with values in the", SoftBreak, Str "range ", Str "[", Str "0, 2^n-1", Str "]", Str ". Mixing signed and unsigned operands follows a conservative", SoftBreak, Str "promotion model: the operands are first promoted to the wider bit-width and", SoftBreak, Str "if any operand is unsigned the operation is performed in the unsigned domain", SoftBreak, Str "of that width."]], [Para [Str "Floating-point types (", Code ("", [], []) "Float' 32", Str ", ", Code ("", [], []) "Float' 64", Str ") correspond to standard", SoftBreak, Str "IEEE-like single- and double-precision floats. Arithmetic on mixed-width", SoftBreak, Str "floats promotes to the wider precision before performing the operation."]]], Header 4 ("promotion-and-result-width", ["unnumbered", "unlisted"], []) [Str "Promotion and Result Width"], BulletList [[Plain [Str "For integer arithmetic, the result width is the maximum of the operand widths", SoftBreak, Str "after promotion; the resulting value is wrapped/clamped to that width as", SoftBreak, Str "described above."]], [Plain [Str "For mixed signed/unsigned arithmetic the operation is performed in the", SoftBreak, Str "unsigned interpretation of the promoted width."]]], Header 4 ("compile-time-requirements-and-partial-evaluation", ["unnumbered", "unlisted"], []) [Str "Compile-time Requirements and Partial Evaluation"], BulletList [[Plain [Str "The bit-width argument (the ", Code ("", [], []) "n", Str " in ", Code ("", [], []) "Int' n", Str ") must be a compile-time", SoftBreak, Str "constant. The reducer treats sized-type aliases (for example ", Code ("", [], []) "Int = Int' 64", Str ")", SoftBreak, Str "as syntactic sugar and reduces type aliases away."]], [Plain [Str "Note: currently the compiler treats sized types primarily as type-level", SoftBreak, Str "annotations and for FFI/representation purposes. Constant arithmetic is", SoftBreak, Str "evaluated by the reducer using Milang's unbounded numeric semantics (or the", SoftBreak, Str "platform default) and is not automatically wrapped/clamped to a target bit", SoftBreak, Str "width. If exact width-limited arithmetic is required, use explicit conversion", SoftBreak, Str "primitives or perform the operation in C via the FFI."]]], Header 4 ("practical-notes", ["unnumbered", "unlisted"], []) [Str "Practical Notes"], BulletList [[Plain [Str "Use sized types when you need explicit control over representation and", SoftBreak, Str "ABI compatibility (FFI interop, binary formats, embedded targets)."]], [Plain [Str "The prelude exposes convenient aliases (", Code ("", [], []) "Int", Str ", ", Code ("", [], []) "UInt", Str ", ", Code ("", [], []) "Float", Str ", ", Code ("", [], []) "Byte", Str ") for", SoftBreak, Str "common widths; you can define your own aliases like ", Code ("", [], []) "Short = Int' 16", Str "."]]], Header 2 ("basic-examples", ["unnumbered", "unlisted"], []) [Str "Basic Examples"], CodeBlock ("", ["milang"], []) "double :: Num : Num
double x = x * 2

add :: Num : Num : Num
add a b = a + b

greeting :: Str : Str
greeting name = \"Hello, \" + name

result = add (double 3) 4
message = greeting \"milang\"
", CodeBlock ("", [""], []) "double = <closure>
add = <closure>
greeting = <closure>
result = 10
message = Hello, milang
", Header 2 ("record-types", ["unnumbered", "unlisted"], []) [Str "Record Types"], Para [Str "Record types describe the shape of a record — field names and their types:"], CodeBlock ("", ["milang"], []) "Point :: {x = Num; y = Num}
", Para [Str "You can use a named record type in function signatures:"], CodeBlock ("", ["milang"], []) "Point :: {x = Num; y = Num}

mkPoint :: Num : Num : Point
mkPoint x y = {x = x; y = y}

p = mkPoint 3 4
", CodeBlock ("", [""], []) "mkPoint = <closure>
p =  {x = 3, y = 4}
", Header 2 ("polymorphism-type-variables", ["unnumbered", "unlisted"], []) [Str "Polymorphism (Type Variables)"], Para [Str "Any unbound identifier in a type expression is automatically a type variable. There is no ", Code ("", [], []) "forall", Str " keyword — just use lowercase names:"], CodeBlock ("", ["milang"], []) "apply :: (a : b) : a : b
apply f x = f x
", Para [Str "Here ", Code ("", [], []) "a", Str " and ", Code ("", [], []) "b", Str " are type variables. ", Code ("", [], []) "apply", Str " works for any function type ", Code ("", [], []) "a : b", Str " applied to an ", Code ("", [], []) "a", Str ", producing a ", Code ("", [], []) "b", Str "."], CodeBlock ("", ["milang"], []) "apply :: (a : b) : a : b
apply f x = f x

double x = x * 2
result = apply double 21
", CodeBlock ("", [""], []) "apply = <closure>
double = <closure>
result = 42
", Header 2 ("adt-types", ["unnumbered", "unlisted"], []) [Str "ADT Types"], Para [Str "You can annotate functions that operate on algebraic data types:"], CodeBlock ("", ["milang"], []) "Shape = {Circle radius; Rect width height}

area :: Shape : Num
area s = s ->
  Circle = 3 * s.radius * s.radius
  Rect = s.width * s.height

a = area (Circle 5)
b = area (Rect 3 4)
", CodeBlock ("", [""], []) "Shape = _module_ {Circle = <closure>, Rect = <closure>}
Circle = <closure>
Rect = <closure>
area = <closure>
a = 75
b = 12
", Header 2 ("the-dual-meaning-of-", ["unnumbered", "unlisted"], []) [Str "The Dual Meaning of ", Code ("", [], []) ":"], Para [Str "The ", Code ("", [], []) ":", Str " symbol is overloaded depending on context:"], BulletList [[Plain [Strong [Str "Value domain:"], Str " cons operator — ", Code ("", [], []) "1 : [2, 3]", Str " builds a list"]], [Plain [Strong [Str "Type domain:"], Str " function arrow — ", Code ("", [], []) "Num : Num : Num", Str " describes a function"]]], Para [Str "This works because ", Code ("", [], []) "::", Str " on its own line clearly marks the boundary between value code and type code. There is never ambiguity."], Header 2 ("type-checking-behavior", ["unnumbered", "unlisted"], []) [Str "Type Checking Behavior"], Para [Str "The type checker is bidirectional: it pushes ", Code ("", [], []) "::", Str " annotations downward and infers types bottom-up. Type errors are reported as errors. Checking is structural — records match by shape (field names and types), not by name. Any record with the right ", Code ("", [], []) "fields", Str " satisfies a record type."]]