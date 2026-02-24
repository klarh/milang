# IO & the World

Milang uses a capability-based IO model. Side effects are not global — they flow through an explicit `world` record that the runtime passes to `main`. If a function never receives `world` (or a sub-record of it), it cannot perform IO.

## Hello World

```milang,run
main world =
  world.io.println "Hello, world!"
```

`main` is the program entry point. It receives `world` and its return value becomes the process exit code.

## The World Record

`world` is a record containing sub-records for different capabilities:

| Path | Description |
|------|-------------|
| `world.io` | Console IO (println, print, readLine) |
| `world.fs.read` | Read-only filesystem (file, exists) |
| `world.fs.write` | Write filesystem (file, append, remove) |
| `world.fs` | Full filesystem access (read + write) |
| `world.process` | Process execution and exit |
| `world.argv` | Command-line arguments (pure — no effect) |
| `world.getEnv` | Read environment variables |

## Console IO

```milang
world.io.println msg          -- print with trailing newline
world.io.print msg            -- print without newline
line = world.io.readLine      -- read one line from stdin
```

## File IO

File operations are split by capability: `world.fs.read` for reading and
`world.fs.write` for writing. This enables fine-grained trait annotations.

```milang
content = world.fs.read.file "data.txt"
world.fs.write.file "out.txt" content
world.fs.write.append "log.txt" "new line\n"
exists = world.fs.read.exists "data.txt"     -- returns 1 or 0
world.fs.write.remove "tmp.txt"
```

## Process

```milang
output = world.process.exec "ls -la"      -- run shell command, return output
world.process.exit 1                       -- exit immediately with status code
```

## Command-Line Arguments and Environment

`world.argv` is an inert list — it does not perform IO, so it is always available:

```milang
main world =
  world.io.println (len world.argv)
```

`world.getEnv` reads an environment variable by name:

```milang
home = world.getEnv "HOME"
```

## Capability Restriction

Because capabilities are just record `fields`, you can restrict what a helper function can do by passing only the sub-record it needs:

```milang,run
greet io = io.println "hello from restricted IO"

main world =
  greet world.io
```

`greet` receives `world.io` and can print, but it structurally cannot access `world.process` — there is no way for it to execute shell commands or exit the process.

## Exit Code

The return value of `main` is used as the process exit code. An integer is used directly; any non-integer value (record, string, etc.) defaults to exit code 0.

```milang,run
main world =
  world.io.println "exiting with code 0"
```

## Script Mode

When a file has no `main` binding that takes a parameter, milang runs in script mode: every top-level binding is evaluated and printed.

```milang,run
x = 6 * 7
y = x + 1
greeting = "hello"
```

This is useful for quick calculations and exploring the language without writing a full `main` function.

## Auto-Monad Spine

You do not need monads or do-notation in milang. The compiler automatically tracks which expressions are *`world`-tainted* (they transitively reference `world`). Impure expressions are guaranteed to execute in the order they appear in the source. Pure expressions can float freely, opening the door for future optimizations. The result is imperative-looking code that is safe and predictable.
