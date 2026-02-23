# Security & Capabilities

Milang's security model is **structural**: if a function does not receive a
capability, it physically cannot use it. There is no global mutable state, no
ambient authority, and no `unsafePerformIO` escape hatch.

## Capability-based IO

All side effects flow through the `world` record that the runtime passes to
`main`. The record contains sub-records — `world.io`, `world.process`, etc. —
each granting access to a specific class of operations.

You restrict a function's power by passing only the sub-record it needs:

```milang
-- greet can print but cannot access the filesystem or exec processes
greet io = io.println "hello"

main world = greet world.io
```

Because `greet` receives `world.io` and nothing else, it is structurally
impossible for it to read files, spawn processes, or access environment
variables. This is milang's equivalent of the principle of least privilege —
enforced by the language, not by convention.

## Remote import pinning

Milang supports importing modules by URL. To prevent supply-chain attacks every
remote import must be *pinned* to a SHA-256 content hash:

```bash
milang pin file.mi
```

This command scans `file.mi` for URL imports, fetches each one, computes its
hash, and records the result. On subsequent compilations the compiler verifies
that the fetched content matches the pinned hash and refuses to proceed if it
does not.

## C FFI security

Milang can call C functions via its FFI. Native C code operates outside the
capability model, so FFI is the one place where the structural guarantee can be
bypassed. Two CLI flags let you lock this down:

- **`--no-ffi`** — disallow all C FFI imports. The program may only use pure
  milang and the built-in `world` capabilities.
- **`--no-remote-ffi`** — allow C FFI in local `.mi` files but forbid it in any
  module imported by URL (and any module transitively imported from that URL
  module). This lets you trust your own native code while sandboxing third-party
  libraries.

Trust is transitive: if your local file imports a remote module `A`, and `A`
imports a relative module `B`, then `B` is also in the remote trust zone and
subject to `--no-remote-ffi`.

## Structural security summary

| Layer | Mechanism |
|---|---|
| Function-level isolation | Pass minimal `world` sub-records |
| Supply-chain integrity | `milang pin` + SHA-256 verification |
| Native code gating | `--no-ffi`, `--no-remote-ffi` |
| Purity tracking | Compiler tracks `world`-tainted expressions; pure code cannot perform IO |

The design principle is simple: the only way to perform a side effect is to hold
the right capability, and capabilities can only travel through explicit function
arguments. The FFI gating flags close the one remaining loophole by controlling
access to native C code.
