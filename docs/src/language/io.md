# IO & the World

<!-- STUB: Cover the capability-based IO model.

  ## The world record
  - `main world = ...` — main receives the world capability record
  - `world` contains sub-records for different capabilities:
    - `world.io` — console and file IO
    - `world.process` — process execution and exit
    - `world.argv` — command-line arguments (inert list, always available)
    - `world.getEnv` — environment variable access
  
  ## Console IO
  - `world.io.println msg` — print with newline
  - `world.io.print msg` — print without newline
  - `world.io.readLine` — read line from stdin (returns string)
  
  ## File IO
  - `world.io.readFile path` — read entire file as string
  - `world.io.writeFile path content` — write string to file
  - `world.io.appendFile path content` — append to file
  - `world.io.exists path` — check if file exists (1/0)
  - `world.io.remove path` — delete file
  
  ## Process
  - `world.process.exec cmd` — run shell command, return output string
  - `world.process.exit code` — exit with status code
  
  ## Capability restriction
  - Pass sub-records to restrict what helpers can do:
    ```
    greet io = io.println "hello"
    main world = greet world.io    -- greet can only do IO, not exec
    ```
  - This is structural: greet literally cannot access world.process
  
  ## Exit code
  - main's return value is the process exit code
  - Integer → used as exit code
  - Non-integer (record, string, etc.) → defaults to 0
  
  ## Script mode
  - If there's no `main` with a parameter, all top-level bindings are
    evaluated and printed (useful for quick calculations)
  
  ## Auto-monad spine
  - The compiler automatically tracks which expressions are "world-tainted"
    (impure — they transitively reference world)
  - Impure expressions are guaranteed to execute in declaration order
  - Pure expressions can float freely (potential for future parallelism)
  - No monads, no do-notation — just write imperative-looking code
  
  Reference: tests/world_basic.mi, tests/file_io.mi, tests/process.mi
-->
