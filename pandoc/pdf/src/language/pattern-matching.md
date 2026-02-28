[Header 1 ("pattern-matching", [], []) [Str "Pattern Matching"], Para [Str "Pattern matching in milang uses the ", Code ("", [], []) "->", Str " operator to dispatch on a value's", SoftBreak, Str "shape. There are no keywords — ", Code ("", [], []) "->", Str " is an expression that evaluates the first", SoftBreak, Str "alternative whose pattern matches."], Header 2 ("basic-syntax", ["unnumbered", "unlisted"], []) [Str "Basic Syntax"], Para [Str "Write ", Code ("", [], []) "expr ->", Str " followed by alternatives. Each alternative is", SoftBreak, Code ("", [], []) "pattern = body", Str ". Alternatives can appear inline (separated by ", Code ("", [], []) ";", Str ") or", SoftBreak, Str "indented on separate lines."], Para [Str "Inline:"], CodeBlock ("", ["milang"], []) "classify x = x -> 0 = \"zero\"; 1 = \"one\"; _ = \"other\"
a = classify 0
b = classify 1
c = classify 42
", CodeBlock ("", [""], []) "classify = <closure>
a = zero
b = one
c = other
", Para [Str "Indented:"], CodeBlock ("", ["milang"], []) "classify x = x ->
  0 = \"zero\"
  1 = \"one\"
  _ = \"other\"
a = classify 0
b = classify 1
c = classify 42
", CodeBlock ("", [""], []) "classify = <closure>
a = zero
b = one
c = other
", Header 2 ("literal-patterns", ["unnumbered", "unlisted"], []) [Str "Literal Patterns"], Para [Str "Integers and strings match by exact value:"], CodeBlock ("", ["milang"], []) "describe n = n ->
  0 = \"zero\"
  1 = \"one\"
  _ = \"many\"
a = describe 0
b = describe 1
c = describe 99
", CodeBlock ("", [""], []) "describe = <closure>
a = zero
b = one
c = many
", Header 2 ("variable-patterns", ["unnumbered", "unlisted"], []) [Str "Variable Patterns"], Para [Str "A lowercase name matches any value and binds it for use in the body:"], CodeBlock ("", ["milang"], []) "myAbs x = x ->
  n | n >= 0 = n
  n = 0 - n
a = myAbs 5
b = myAbs (0 - 3)
", CodeBlock ("", [""], []) "myAbs = <closure>
a = 5
b = 3
", Header 2 ("wildcard", ["unnumbered", "unlisted"], []) [Str "Wildcard"], Para [Code ("", [], []) "_", Str " matches any value without binding it. Use it for catch-all branches:"], CodeBlock ("", ["milang"], []) "isZero x = x ->
  0 = 1
  _ = 0
a = isZero 0
b = isZero 7
", CodeBlock ("", [""], []) "isZero = <closure>
a = 1
b = 0
", Header 2 ("constructor-tag-patterns", ["unnumbered", "unlisted"], []) [Str "Constructor Tag Patterns"], Para [Str "Match on a tagged record's constructor. After matching, the scrutinee's ", Code ("", [], []) "fields", SoftBreak, Str "are accessible through dot notation:"], CodeBlock ("", ["milang"], []) "Shape = {Circle radius; Rect width height}
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
", Para [Str "With named-field destructuring in the pattern, ", Code ("", [], []) "fields", Str " are bound directly:"], CodeBlock ("", ["milang"], []) "Shape = {Circle radius; Rect width height}
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
", Header 2 ("list-patterns", ["unnumbered", "unlisted"], []) [Str "List Patterns"], Para [Str "Match a list by its elements. ", Code ("", [], []) "[a, b, c]", Str " matches a list of exactly three", SoftBreak, Str "elements. ", Code ("", [], []) "[first, ...rest]", Str " matches one or more elements, binding the ", Code ("", [], []) "tail", Str ":"], CodeBlock ("", ["milang"], []) "xs = [10, 20, 30, 40]
result = xs ->
  [a, b, ...rest] = {first = a; second = b; rest = rest}
  [] = {first = 0; second = 0; rest = []}
", CodeBlock ("", [""], []) "xs = [10, 20, 30, 40]
result =  {first = 10, second = 20, rest = [30, 40]}
", Para [Str "An empty-list pattern matches ", Code ("", [], []) "[]", Str " (", Code ("", [], []) "Nil", Str "):"], CodeBlock ("", ["milang"], []) "isEmpty xs = xs ->
  [] = \"empty\"
  _ = \"non-empty\"
a = isEmpty []
b = isEmpty [1]
", CodeBlock ("", [""], []) "isEmpty = <closure>
a = empty
b = non-empty
", Header 2 ("guards", ["unnumbered", "unlisted"], []) [Str "Guards"], Para [Str "A guard adds a condition to an alternative using ", Code ("", [], []) "| condition", Str " before the ", Code ("", [], []) "=", Str ".", SoftBreak, Str "The alternative only matches when both the pattern and the guard are satisfied:"], CodeBlock ("", ["milang"], []) "classify x = x ->
  n | n < 0 = \"negative\"
  n | n == 0 = \"zero\"
  _ = \"positive\"
a = classify (0 - 5)
b = classify 0
c = classify 10
", CodeBlock ("", [""], []) "classify = <closure>
a = negative
b = zero
c = positive
", Header 2 ("guard-only-matching", ["unnumbered", "unlisted"], []) [Str "Guard-Only Matching"], Para [Str "When every alternative uses only a guard (no structural pattern), you can", SoftBreak, Str "write guards directly after ", Code ("", [], []) "->", Str ":"], CodeBlock ("", ["milang"], []) "classify x = x ->
  | x < 0 = \"negative\"
  | x == 0 = \"zero\"
  | _ = \"positive\"
a = classify (0 - 5)
b = classify 0
c = classify 10
", CodeBlock ("", [""], []) "classify = <closure>
a = negative
b = zero
c = positive
", Header 2 ("combined-pattern--guard", ["unnumbered", "unlisted"], []) [Str "Combined Pattern + Guard"], Para [Str "A constructor or literal pattern can be paired with a guard:"], CodeBlock ("", ["milang"], []) "Shape = {Circle radius; Rect width height}
safeArea shape ->
  Circle {radius} | radius > 0 = 3.14 * radius * radius
  _ = 0
a = safeArea (Circle 5)
b = safeArea (Circle 0)
c = safeArea (Rect 3 4)
", CodeBlock ("", [""], []) "Shape = _module_ {Circle = <closure>, Rect = <closure>}
Circle = <closure>
Rect = <closure>
safeArea = <closure>
a = 78.5
b = 0
c = 0
", Header 2 ("match-in-function-bindings", ["unnumbered", "unlisted"], []) [Str "Match in Function Bindings"], Para [Str "The ", Code ("", [], []) "f param ->", Str " sugar defines a function that immediately matches its ", Code ("", [], []) "last", SoftBreak, Str "parameter, avoiding an extra ", Code ("", [], []) "= param ->", Str " layer:"], CodeBlock ("", ["milang"], []) "Shape = {Circle radius; Rect width height}
describe label shape ->
  Circle = label + \": circle\"
  Rect = label + \": rect\"
  _ = label + \": unknown\"
a = describe \"shape\" (Circle 5)
b = describe \"shape\" (Rect 3 4)
", CodeBlock ("", [""], []) "Shape = _module_ {Circle = <closure>, Rect = <closure>}
Circle = <closure>
Rect = <closure>
describe = <closure>
a = shape: circle
b = shape: rect
", Header 2 ("exhaustiveness", ["unnumbered", "unlisted"], []) [Str "Exhaustiveness"], Para [Str "When the compiler can determine the type of a scrutinee (e.g., from a ", Code ("", [], []) "::", Str " type annotation), it checks that all constructors of a union type are covered. If any constructor is missing and there is no wildcard ", Code ("", [], []) "_", Str " catch-all, the compiler emits a warning:"], CodeBlock ("", ["text"], []) "warning: non-exhaustive patterns for Shape — missing: Rect
", Para [Str "To silence the warning, either cover all constructors explicitly or add a wildcard branch:"], CodeBlock ("", ["milang"], []) "area s = s ->
  Circle = 3.14 * s.radius * s.radius
  _ = 0  -- catch-all for all other shapes
", Para [Str "Exhaustiveness checking only triggers when the scrutinee type is a known union type from a ", Code ("", [], []) "::", Str " annotation. Unannotated scrutinees without a catch-all will compile without warning but may fail at runtime if an unmatched constructor is encountered."], Header 2 ("matching-maybe", ["unnumbered", "unlisted"], []) [Str "Matching Maybe"], CodeBlock ("", ["milang"], []) "matchMaybe m = m ->
  Just {val} = \"Just(\" + toString val + \")\"
  Nothing = \"Nothing\"

main world =
  world.io.println (matchMaybe (Just 5))
  world.io.println (matchMaybe Nothing)
", CodeBlock ("", [""], []) "Just(5)
Nothing
"]