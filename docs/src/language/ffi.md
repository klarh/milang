# C FFI

Milang can call C functions directly by importing a `.h` header file. The compiler parses the header, extracts function signatures, and maps C types to milang types. At code generation time the header is `#include`d and calls are emitted inline — no wrapper overhead.

## Type Mapping

| C type | Milang type | Notes |
|--------|-------------|-------|
| `int`, `long`, `int64_t`, `size_t` | `Int` | All integer types map to 64-bit int |
| `double` | `Float` | 64-bit float |
| `float` | `Float` | 32-bit float (promoted automatically in mixed arithmetic) |
| `char*` | `Str` | C strings |
| `void*`, opaque struct pointers | Opaque handle | Use with `gc_manage` for cleanup |
| `void` return | `Int` (0) | Void functions return 0 |
| `typedef struct { ... } Name` | Record | Fields become record fields |
| `typedef enum { ... } Name` | `Int` constants | Enum members exposed on module record |
| `typedef ret (*Name)(params)` | Callback | Pass milang functions as C callbacks |


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

1. The import resolver reads the `.h` file and extracts function declarations, struct definitions, enum constants, and function pointer typedefs.
2. Each C function becomes an internal `CFunction` AST node with its milang type signature.
3. Struct and enum type names are resolved so they can be used as parameter and return types.
4. Enum constants become `Int` bindings on the module record.
5. During C code generation the header is `#include`d and calls are emitted as direct C function calls.
6. Any associated source files are compiled and linked automatically.

## Structs by Value

C structs defined with `typedef struct` or `struct Name` are automatically mapped to milang records. Fields are accessible by name:

```c
// vec.h
typedef struct { double x; double y; } Vec2;
Vec2 vec2_add(Vec2 a, Vec2 b);
double vec2_dot(Vec2 a, Vec2 b);
```

```milang
v = import' "vec.h" ({src = "vec.c"})

a = {x = 1.0; y = 2.0}
b = {x = 3.0; y = 4.0}
result = v.vec2_add a b    -- {x = 4.0, y = 6.0}
dot = v.vec2_dot a b       -- 11.0
```

Records passed to struct-taking functions are converted to C structs using C99 compound literals. Struct return values are converted to milang records with the same field names.

## Enum Constants

C enum definitions in headers are exposed as `Int` constants on the module record:

```c
// color.h
typedef enum { RED = 0, GREEN = 1, BLUE = 2 } Color;
int color_value(Color c);
```

```milang
c = import' "color.h" ({src = "color.c"})

c.RED                    -- 0
c.GREEN                  -- 1
c.color_value c.BLUE     -- uses enum constant as argument
```

Both `typedef enum { ... } Name;` and `enum Name { ... };` are supported. Auto-incrementing values work as in C.

## Callbacks (Function Pointers)

Milang functions can be passed to C functions that expect function pointers. Define the callback type with `typedef`:

```c
// callback.h
typedef long (*IntFn)(long);
long apply_fn(IntFn f, long x);
long apply_twice(IntFn f, long x);
```

```milang
cb = import' "callback.h" ({src = "callback.c"})

cb.apply_fn (\x -> x * 2) 21        -- 42
cb.apply_twice (\x -> x + 1) 0      -- 2

-- Named functions work too
square x = x * x
cb.apply_fn square 7                 -- 49
```

The compiler generates a trampoline that converts between C calling conventions and milang's closure-based evaluation. Multi-parameter callbacks are supported:

```c
typedef long (*BinFn)(long, long);
long fold_range(BinFn f, long init, long n);
```

```milang
add_fn acc i = acc + i
cb.fold_range add_fn 0 10    -- sum of 0..9 = 45
```

Callbacks are pinned as GC roots, so they remain valid even if the C library stores and calls them later (e.g., event handlers in GUI frameworks).

## Security Considerations

C code bypasses milang's capability model — a C function can perform arbitrary IO, allocate memory, or call system APIs regardless of what capabilities were passed to the milang caller. Use the following flags to restrict FFI access:

- **`--no-ffi`** — disallow all C header imports. Any `import "*.h"` will fail.
- **`--no-remote-ffi`** — allow local `.mi` files to use C FFI, but prevent URL-imported modules from importing C headers. This stops remote code from escaping the capability sandbox through native calls.

These flags are especially important when running untrusted or third-party milang code.

## Memory Management for FFI Pointers

By default, pointers returned from C functions are **unmanaged** — they become `MI_POINTER` values that are never freed. For short-lived programs this is fine, but long-running programs will leak memory.

### Automatic cleanup with `gc_manage`

Use the `gc_manage` builtin to associate a pointer with a finalizer function. The garbage collector will automatically call the finalizer when the value becomes unreachable:

```milang
ffi = import' "mylib.h" ({src = "mylib.c"})

-- Wrap the pointer with its free function
obj = gc_manage (ffi.myobj_create 42) ffi.myobj_free

-- Use normally — FFI functions accept managed pointers transparently
val = ffi.myobj_read obj

-- No manual free needed! The GC handles cleanup.
```

`gc_manage` takes two arguments:
1. A pointer value (from an FFI allocation function)
2. A native function (the FFI free/destructor function)

It returns an `MI_MANAGED` value that behaves identically to a regular pointer in FFI calls — all existing FFI functions work without modification.

### When to use `gc_manage`

- **Use it** for objects that your code allocates and should own: arrays, buffers, file handles, database connections.
- **Don't use it** for pointers returned by C functions that manage their own lifetime (e.g., `stdin`, shared library handles).

### C-level registration

FFI implementors who prefer to register finalizers in C can use `mi_managed()` directly:

```c
// In your FFI .c file — declare the runtime function
extern MiVal mi_managed(void *ptr, void (*finalizer)(void*));

MyObj* myobj_create(long val) {
    MyObj *obj = malloc(sizeof(MyObj));
    obj->value = val;
    // Register with GC — milang code gets an MI_MANAGED value automatically
    return obj;  // still returns raw pointer; use gc_manage from milang instead
}
```

> **Note:** When using C-level `mi_managed()`, the FFI wrapper function should return `MiVal` directly rather than a raw pointer. In most cases, using `gc_manage` from milang code is simpler.

### How the GC works

Milang uses a mark-sweep garbage collector for runtime-allocated environments (`MiEnv`) and managed pointers:

- **Init-time allocations** (prelude setup, AST construction) use a bump-allocated arena and are never freed.
- **Eval-time allocations** (created during program execution) use a malloc-based pool with a free list.
- The GC runs automatically every 100K environment allocations.
- During the **mark** phase, the GC traces all reachable values from the current environment root, including closures, managed pointers, and pinned callback closures.
- During the **sweep** phase, unreachable environments are returned to the pool, and unreachable managed pointers have their finalizers called.

For tail-recursive programs, memory stays bounded — the GC reclaims environments from completed iterations.
