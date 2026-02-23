# C FFI

Milang can call C functions directly by importing a `.h` header file. The compiler parses the header, extracts function signatures, and maps C types to milang types (`int`/`long` -> `Int`, `double` -> `Float`, `float` -> `Float' 32`, `char*` -> `Str`). At code generation time the header is `#include`d and calls are emitted inline — no wrapper overhead.

<!-- FFI mapping for sized types

Sized milang types map to fixed-width C integer types in the FFI layer for
predictable ABI compatibility:

- `Int' 8`  -> `int8_t`
- `Int' 16` -> `int16_t`
- `Int' 32` -> `int32_t`
- `Int' 64` -> `int64_t`

- `UInt' 8`  -> `uint8_t`
- `UInt' 16` -> `uint16_t`
- `UInt' 32` -> `uint32_t`
- `UInt' 64` -> `uint64_t`

Floating milang types map to the natural C floating types for the precision
requested (e.g. `Float' 32` corresponds to `float`, `Float' 64` to `double`).

When importing C headers the compiler attempts to match C signatures to
milang types. If a direct mapping is not available the import step will raise
an error and prompt you to provide an explicit shim or a compatible signature.
-->


## Importing C Headers

Import a system header the same way you import a `.mi` file:

```milang
m = import "math.h"

result = m.sin 1.0
root = m.sqrt 144.0
```

The result is a record whose `fields` are the C functions declared in the header. Use dot notation to call them.

## Selective Import with `import'`

If you only need a few functions, or need to attach compilation options, use the `import'` form:

```milang
m = import' "math.h" ({})
result = m.cos 0.0
```

## Associating C Source Files

For your own C libraries, tell the compiler which source files to compile alongside the generated code:

```milang
lib = import' "mylib.h" ({src = "mylib.c"})
answer = lib.add_ints 3 4
```

The `src` field takes a single source file path (relative to the importing `.mi` file).

## Advanced Options

The options record passed to `import'` supports several `fields`:

| Field | Type | Description |
|-------|------|-------------|
| `src` | `Str` | Single C source file to compile |
| `sources` | `List` | Multiple source files: `["a.c", "b.c"]` |
| `flags` | `Str` | Additional compiler flags (e.g. `"-O2 -Wall"`) |
| `include` | `Str` | Additional include directory |
| `pkg` | `Str` | pkg-config package name — auto-discovers flags and includes |

Example with multiple options:

```milang
lib = import' "mylib.h" ({
  sources = ["mylib.c", "helpers.c"]
  flags = "-O2"
  include = "vendor/include"
})
```

Using pkg-config for a system library:

```milang
json = import' "json-c/json.h" ({pkg = "json-c"})
```

## How It Works

1. The import resolver reads the `.h` file and extracts function declarations.
2. Each C function becomes an internal `CFunction` AST node with its milang type signature.
3. During C code generation the header is `#include`d and calls are emitted as direct C function calls.
4. Any associated source files are compiled and linked automatically.

## Security Considerations

C code bypasses milang's capability model — a C function can perform arbitrary IO, allocate memory, or call system APIs regardless of what capabilities were passed to the milang caller. Use the following flags to restrict FFI access:

- **`--no-ffi`** — disallow all C header imports. Any `import "*.h"` will fail.
- **`--no-remote-ffi`** — allow local `.mi` files to use C FFI, but prevent URL-imported modules from importing C headers. This stops remote code from escaping the capability sandbox through native calls.

These flags are especially important when running untrusted or third-party milang code.
