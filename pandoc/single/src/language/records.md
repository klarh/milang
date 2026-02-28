[Header 1 ("records--adts", [], []) [Str "Records & ADTs"], Para [Str "Records are milang's primary data structure. They hold named ", Code ("", [], []) "fields", Str ", support", SoftBreak, Str "structural updates, and form the basis of algebraic data types (ADTs)."], Header 2 ("record-literals", ["unnumbered", "unlisted"], []) [Str "Record Literals"], Para [Str "A record is a set of ", Code ("", [], []) "field = value", Str " pairs inside braces, separated by ", Code ("", [], []) ";", Str " or", SoftBreak, Str "newlines:"], CodeBlock ("", ["milang"], []) "point = {x = 3; y = 4}
person = {name = \"Alice\"; age = 30}
", CodeBlock ("", [""], []) "point =  {x = 3, y = 4}
person =  {name = Alice, age = 30}
", Header 2 ("field-access", ["unnumbered", "unlisted"], []) [Str "Field Access"], Para [Str "Use dot notation to read a field. Dots chain for nested records."], CodeBlock ("", ["milang"], []) "point = {x = 3; y = 4}
a = point.x
b = point.y
", CodeBlock ("", [""], []) "point =  {x = 3, y = 4}
a = 3
b = 4
", Header 2 ("positional-access", ["unnumbered", "unlisted"], []) [Str "Positional Access"], Para [Str "Fields can also be accessed by declaration order using ", Code ("", [], []) "_0", Str ", ", Code ("", [], []) "_1", Str ", etc.:"], CodeBlock ("", ["milang"], []) "pair = {first = \"hello\"; second = \"world\"}
a = pair._0
b = pair._1
", CodeBlock ("", [""], []) "pair =  {first = hello, second = world}
a = hello
b = world
", Header 2 ("record-update", ["unnumbered", "unlisted"], []) [Str "Record Update"], Para [Str "The ", Code ("", [], []) "<-", Str " operator creates a new record with selected ", Code ("", [], []) "fields", Str " replaced. Fields", SoftBreak, Str "not mentioned are carried over unchanged."], CodeBlock ("", ["milang"], []) "base = {x = 1; y = 2; z = 3}
moved = base <- {x = 10; z = 30}
", CodeBlock ("", [""], []) "base =  {x = 1, y = 2, z = 3}
moved =  {x = 10, y = 2, z = 30}
", Header 2 ("destructuring", ["unnumbered", "unlisted"], []) [Str "Destructuring"], Para [Str "Bind ", Code ("", [], []) "fields", Str " from a record directly into the current scope. Use ", Code ("", [], []) "{field}", Str " for", SoftBreak, Str "same-name bindings, or ", Code ("", [], []) "{local = field}", Str " to rename:"], CodeBlock ("", ["milang"], []) "point = {x = 3; y = 4}
{x; y} = point
sum = x + y
", CodeBlock ("", [""], []) "point =  {x = 3, y = 4}
_destruct_23 =  {x = 3, y = 4}
x = 3
y = 4
", Para [Str "Renaming during destructuring:"], CodeBlock ("", ["milang"], []) "point = {x = 3; y = 4}
{myX = x; myY = y} = point
result = myX + myY
", CodeBlock ("", [""], []) "point =  {x = 3, y = 4}
_destruct_23 =  {x = 3, y = 4}
myX = 3
myY = 4
result = 7
", Header 2 ("scope-as-record", ["unnumbered", "unlisted"], []) [Str "Scope-as-Record"], Para [Str "When a function body has no explicit result expression — just indented", SoftBreak, Str "bindings — the named bindings are collected into an implicit record:"], CodeBlock ("", ["milang"], []) "makeVec x y =
  magnitude = x + y
  product = x * y
v = makeVec 3 4
", CodeBlock ("", [""], []) "makeVec = <closure>
v =  {magnitude = 7, product = 12}
", Para [Str "Bare expressions (not bound to a name) execute for their side effects and are", SoftBreak, Strong [Str "not"], Str " included in the returned record. This is how ", Code ("", [], []) "main", Str " works — see the", SoftBreak, Link ("", [], []) [Str "Scopes"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/single/src/language/scopes.md#scopes--bindings", ""), Str " chapter."], Header 2 ("adts-algebraic-data-types", ["unnumbered", "unlisted"], []) [Str "ADTs (Algebraic Data Types)"], Para [Str "An uppercase name bound to braces containing uppercase constructors declares a", SoftBreak, Str "tagged union:"], CodeBlock ("", ["milang"], []) "Shape = {Circle radius; Rect width height; Point}
c = Circle 5
r = Rect 3 4
p = Point
", CodeBlock ("", [""], []) "Shape = _module_ {Circle = <closure>, Rect = <closure>, Point = Point {}}
Circle = <closure>
Rect = <closure>
Point = Point {}
c = Circle {radius = 5}
r = Rect {width = 3, height = 4}
p = Point {}
", Para [Str "Each constructor becomes a function that produces a tagged record. Zero-field", SoftBreak, Str "constructors (like ", Code ("", [], []) "Point", Str " above) are plain tagged records with no arguments."], Para [Str "Constructors are also available namespaced under the type name", SoftBreak, Str "(e.g. ", Code ("", [], []) "Shape.Circle", Str ")."], Header 2 ("constructors-as-functions", ["unnumbered", "unlisted"], []) [Str "Constructors as Functions"], Para [Str "Because constructors are just functions, they work naturally with ", Code ("", [], []) "map", Str " and", SoftBreak, Str "other higher-order functions:"], CodeBlock ("", ["milang"], []) "values = map (\\x -> Just x) [1, 2, 3]
", CodeBlock ("", [""], []) "values = [Just {val = 1}, Just {val = 2}, Just {val = 3}]
", Header 2 ("pattern-matching-on-tags", ["unnumbered", "unlisted"], []) [Str "Pattern Matching on Tags"], Para [Str "Use the ", Code ("", [], []) "->", Str " operator to match on a value's constructor ", Code ("", [], []) "tag", Str ". After a ", Code ("", [], []) "tag", SoftBreak, Str "matches, the record's ", Code ("", [], []) "fields", Str " are accessible via dot notation or destructuring:"], CodeBlock ("", ["milang"], []) "Shape = {Circle radius; Rect width height}
area shape = shape ->
  Circle = 3.14 * shape.radius * shape.radius
  Rect = shape.width * shape.height
a = area (Circle 5)
b = area (Rect 3 4)
", CodeBlock ("", [""], []) "Shape = _module_ {Circle = <closure>, Rect = <closure>}
Circle = <closure>
Rect = <closure>
area = <closure>
a = 78.5
b = 12
", Para [Str "Named-field destructuring in alternatives:"], CodeBlock ("", ["milang"], []) "Shape = {Circle radius; Rect width height}
area shape ->
  Circle {radius} = 3.14 * radius * radius
  Rect {width; height} = width * height
a = area (Circle 5)
b = area (Rect 3 4)
", CodeBlock ("", [""], []) "Shape = _module_ {Circle = <closure>, Rect = <closure>}
Circle = <closure>
Rect = <closure>
area = <closure>
a = 78.5
b = 12
", Para [Str "See the ", Link ("", [], []) [Str "Pattern Matching"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/single/src/language/pattern-matching.md#pattern-matching", ""), Str " chapter for the full range of", SoftBreak, Str "patterns, guards, and list matching."], Header 2 ("record-introspection", ["unnumbered", "unlisted"], []) [Str "Record Introspection"], Para [Str "Several built-in functions let you inspect records dynamically:"], Table ("", [], []) (Caption Nothing []) [(AlignDefault, ColWidthDefault), (AlignDefault, ColWidthDefault)] (TableHead ("", [], []) [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Function"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Returns"]]]]) [(TableBody ("", [], []) (RowHeadColumns 0) [] [Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "fields r"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "List of ", Code ("", [], []) "{name, value}", Str " records (", Code ("", [], []) "[]", Str " for non-records)"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "fieldNames r"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "List of field-name strings"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "tag r"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "Constructor ", Code ("", [], []) "tag", Str " string, or ", Code ("", [], []) "\"\"", Str " for untagged values"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "getField r \"name\""]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "Just value", Str " if present, ", Code ("", [], []) "Nothing", Str " if missing"]]], Row ("", [], []) [Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Code ("", [], []) "setField r \"name\" val"]], Cell ("", [], []) AlignDefault (RowSpan 0) (ColSpan 0) [Plain [Str "New record with field set"]]]])] (TableFoot ("", [], []) [])]