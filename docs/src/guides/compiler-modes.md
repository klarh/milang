# Compiler Modes

<!-- STUB: Cover the different ways to use the milang compiler.

  ## milang run
  ```bash
  ./milang run file.mi [args...]
  ```
  - Compiles to C, compiles C with gcc, runs the binary
  - Passes command-line args via world.argv
  - Cleans up temp files after execution
  - Exit code = main's return value (if int)
  
  ## milang compile
  ```bash
  ./milang compile file.mi output.c
  ```
  - Emits a standalone C file (runtime embedded)
  - Compile manually: `gcc output.c -o program -lm`
  - The C file is self-contained — no external runtime needed
  
  ## milang dump
  ```bash
  ./milang dump file.mi
  ```
  - Shows the reduced AST after partial evaluation
  - Useful for debugging: see what the compiler reduced
  - Shows all annotation domains (::, :~, :?)
  
  ## milang repl
  ```bash
  ./milang repl
  ```
  - Interactive read-eval-print loop
  - See REPL chapter for details
  
  ## Script mode (no main)
  - If a file has no `main` binding with a parameter, it runs in script mode
  - All top-level bindings are evaluated and printed
  ```
  x = 6 * 7
  y = x + 1
  ```
  Output: `x = 42` and `y = 43`
  
  Reference: try each mode with a simple .mi file
-->
