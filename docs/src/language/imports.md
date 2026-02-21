# Imports & Modules

<!-- STUB: Cover the import system.

  ## Local imports
  ```
  import "other.mi"
  ```
  - Imports all top-level bindings from other.mi into current scope
  - Relative to the importing file's directory
  
  ## URL imports
  ```
  import "https://example.com/lib.mi"
  ```
  - Downloads and caches the module
  - Should include sha256 pin for security (see Security chapter)
  
  ## Circular imports
  - Supported — the import resolver detects cycles
  - Circular bindings are marked lazy to break the cycle
  
  ## Diamond imports
  - Handled correctly — a module imported from multiple paths is loaded once
  
  ## Module structure
  - A .mi file's top-level bindings form its public interface
  - No explicit export/import lists — everything is public
  - Use naming conventions for "private" helpers (e.g. underscore prefix)
  
  ## C FFI imports (see C FFI chapter)
  ```
  import "math.h"
  import' "math.h" (sin, cos, sqrt)
  ```
  
  Reference: tests/import_basic.mi, tests/import_circular.mi,
             tests/import_diamond.mi, tests/import_url.mi
-->
