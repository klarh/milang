# C FFI

<!-- STUB: Cover foreign function interface with C.

  ## Importing C headers
  ```
  import "math.h"                      -- import all detected functions
  import' "math.h" (sin, cos, sqrt)    -- selective import
  ```
  - The compiler parses .h files to extract function signatures
  - Supports: function declarations with standard C types
  - Maps C types to milang: int/long → Int, double/float → Float, char* → String
  
  ## How it works
  - Import resolution reads the .h file and extracts function signatures
  - Each C function becomes a CFunction AST node
  - Codegen emits direct C calls (no wrapper overhead)
  - The .h file is #included in the generated C output
  
  ## C source compilation
  - For multi-file C projects, use structured import:
    ```
    import' "mylib.h" (myFunc) {
      src = "mylib.c"
      flags = "-O2"
    }
    ```
  - `src` — C source file to compile alongside
  - `sources` — list of source files: `["a.c", "b.c"]`
  - `flags` — additional compiler flags
  - `include` — additional include directories
  - `pkg` — pkg-config package name (auto-discovers flags)
  
  ## Security
  - Local .mi files can use C FFI freely
  - URL-imported modules: FFI allowed by default
  - `--no-remote-ffi` flag restricts URL imports from using C FFI
  - `--no-ffi` disallows ALL C imports
  - FFI bypasses milang's capability model — C code can do anything
  
  Reference: tests/ffi.mi, tests/ffi_math.mi
-->
