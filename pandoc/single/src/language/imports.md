[Header 1 ("imports--modules", [], []) [Str "Imports & Modules"], Para [Str "Every ", Code ("", [], []) ".mi", Str " file is a module. Importing a module evaluates it and returns a record containing all of its top-level bindings. You bind that record to a name and access its members with dot notation — no special export lists or visibility modifiers."], Header 2 ("local-imports", ["unnumbered", "unlisted"], []) [Str "Local Imports"], Para [Str "Use ", Code ("", [], []) "import", Str " with a file path (relative to the importing file's directory):"], CodeBlock ("", ["milang"], []) "math = import \"lib/mymath.mi\"

area = math.circle_area 5
", Para [Str "The result of ", Code ("", [], []) "import", Str " is a record, so ", Code ("", [], []) "math.circle_area", Str " and ", Code ("", [], []) "math.pi", Str " access individual bindings from the imported file."], Header 2 ("a-complete-example", ["unnumbered", "unlisted"], []) [Str "A Complete Example"], CodeBlock ("", ["milang"], []) "-- Suppose lib/mymath.mi contains:
--   pi = 3.14159
--   circle_area r = pi * r * r

-- We can inline the same definitions here to demonstrate:
pi = 3.14159
circle_area r = pi * r * r

circumference r = 2 * pi * r
", CodeBlock ("", [""], []) "pi = 3.14159
circle_area = <closure>
circumference = <closure>
", Header 2 ("url-imports", ["unnumbered", "unlisted"], []) [Str "URL Imports"], Para [Str "Remote modules are imported the same way — just use a URL:"], CodeBlock ("", ["milang"], []) "collections = import \"https://example.com/milang-stdlib/collections.mi\"

total = collections.sum [1, 2, 3]
", Para [Str "The compiler downloads the file and caches it locally. On subsequent runs the cached version is used."], Header 2 ("pinned-imports-with-import", ["unnumbered", "unlisted"], []) [Str "Pinned Imports with ", Code ("", [], []) "import'"], Para [Str "URL imports must be pinned by their SHA-256 hash using the ", Code ("", [], []) "import'", Str " form:"], CodeBlock ("", ["milang"], []) "lib = import' \"https://example.com/lib.mi\" ({sha256 = \"a1b2c3...\"})
", Para [Str "If the downloaded content does not match the hash, compilation fails. The ", Code ("", [], []) "milang pin", Str " command computes the hash for you:"], CodeBlock ("", ["bash"], []) "$ milang pin https://example.com/lib.mi
sha256 = \"a1b2c3d4e5f6...\"
", Header 2 ("c-header-imports", ["unnumbered", "unlisted"], []) [Str "C Header Imports"], Para [Str "When the path ends in ", Code ("", [], []) ".h", Str ", the compiler parses the C header and exposes its functions as milang bindings. See the ", Link ("", [], []) [Str "C FFI"] ("/home/matthew/dev/milang-copilot/docs-out/pandoc/single/src/language/ffi.md#c-ffi", ""), Str " chapter for details."], CodeBlock ("", ["milang"], []) "m = import \"math.h\"
result = m.sin 1.0
", Para [Str "You can also associate C source files and compiler flags with ", Code ("", [], []) "import'", Str ":"], CodeBlock ("", ["milang"], []) "lib = import' \"mylib.h\" ({src = \"mylib.c\"})
answer = lib.add_ints 3 4
", Header 2 ("circular-imports", ["unnumbered", "unlisted"], []) [Str "Circular Imports"], Para [Str "Milang supports circular imports. When module A imports module B and B imports A, the resolver detects the cycle and marks the circular bindings as lazy (thunks) to break the dependency. Both modules load correctly and can reference each other's bindings."], Header 2 ("diamond-imports", ["unnumbered", "unlisted"], []) [Str "Diamond Imports"], Para [Str "If two modules both import the same third module, it is loaded and evaluated only once. The two importers share the same record, so there is no duplication or inconsistency."], Header 2 ("visibility", ["unnumbered", "unlisted"], []) [Str "Visibility"], Para [Str "All top-level bindings in a ", Code ("", [], []) ".mi", Str " file are exported — there is no private/public distinction. If you want to signal that a binding is an internal helper, use a naming convention such as an underscore prefix (", Code ("", [], []) "_helper", Str "), but the compiler does not enforce this."]]