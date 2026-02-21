# Hello World

<!-- STUB: Walk through creating and running a first program.
  
  1. Create hello.mi:
     ```
     main world =
       world.io.println "Hello, world!"
       0
     ```
  2. Run it: `./milang run hello.mi`
  3. Explain what happened:
     - `main` is the entry point — it receives `world`, the capability record
     - `world.io.println` is a function accessed from world's io capabilities
     - The bare expression `world.io.println "Hello, world!"` executes for effect
     - `0` is the exit code (main's return value becomes the process exit code)
  
  4. Show a pure example without IO:
     ```
     x = 6 * 7
     ```
     Run with `./milang run` — prints `x = 42` (script mode, no main)
  
  5. Show compilation to C:
     ```
     ./milang compile hello.mi hello.c
     gcc hello.c -o hello -lm
     ./hello
     ```
  
  6. Show the REPL:
     ```
     ./milang repl
     > 2 + 3
     5
     > f x = x * 2
     > f 21
     42
     ```
-->
