# Imports & Modules

Every `.mi` file is a module. Importing a module evaluates it and returns a record containing all of its top-level bindings. You bind that record to a name and access its members with dot notation — no special export lists or visibility modifiers.

## Local Imports

Use `import` with a file path (relative to the importing file's directory):

```milang
math = import "lib/mymath.mi"

area = math.circle_area 5
```

The result of `import` is a record, so `math.circle_area` and `math.pi` access individual bindings from the imported file.

## A Complete Example

```milang,run
-- Suppose lib/mymath.mi contains:
--   pi = 3.14159
--   circle_area r = pi * r * r

-- We can inline the same definitions here to demonstrate:
pi = 3.14159
circle_area r = pi * r * r

circumference r = 2 * pi * r
```

## URL Imports

Remote modules are imported the same way — just use a URL:

```milang
collections = import "https://example.com/milang-stdlib/collections.mi"

total = collections.sum [1, 2, 3]
```

The compiler downloads the file and caches it locally. On subsequent runs the cached version is used.

## Pinned Imports with `import'`

URL imports must be pinned by their SHA-256 hash using the `import'` form:

```milang
lib = import' "https://example.com/lib.mi" ({sha256 = "a1b2c3..."})
```

If the downloaded content does not match the hash, compilation fails. The `milang pin` command computes the hash for you:

```bash
$ milang pin https://example.com/lib.mi
sha256 = "a1b2c3d4e5f6..."
```

## C Header Imports

When the path ends in `.h`, the compiler parses the C header and exposes its functions as milang bindings. See the [C FFI](./ffi.md) chapter for details.

```milang
m = import "math.h"
result = m.sin 1.0
```

You can also associate C source files and compiler flags with `import'`:

```milang
lib = import' "mylib.h" ({src = "mylib.c"})
answer = lib.add_ints 3 4
```

## Circular Imports

Milang supports circular imports. When module A imports module B and B imports A, the resolver detects the cycle and marks the circular bindings as lazy (thunks) to break the dependency. Both modules load correctly and can reference each other's bindings.

## Diamond Imports

If two modules both import the same third module, it is loaded and evaluated only once. The two importers share the same record, so there is no duplication or inconsistency.

## Visibility

All top-level bindings in a `.mi` file are exported — there is no private/public distinction. If you want to signal that a binding is an internal helper, use a naming convention such as an underscore prefix (`_helper`), but the compiler does not enforce this.
