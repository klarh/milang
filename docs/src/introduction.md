# The Milang Programming Language

Milang is a minimalist functional programming language with:

- **Zero keywords** — everything is a function or operator, including `if`
- **Haskell-like syntax** — clean, whitespace-sensitive, expression-oriented
- **Partial evaluation** as the core compilation model — the compiler reduces your program as far as possible at compile time, then emits C code for what remains
- **Capability-based IO** — side effects flow through an explicit `world` value
- **Five annotation domains** — types (`::`) , traits (`:~`), docs (`:?`), parse declarations (`:!`), and values (`=`)

<!-- STUB: Write 2-3 paragraphs expanding on the philosophy:
  - "Extreme simplicity" — no special forms, everything composes uniformly
  - Partial evaluation means the compiler IS the optimizer
  - Capability model means security is structural, not bolted on
  - The language targets C, so it's fast and portable
  Look at showcase.mi in the repo root for a good example program.
  Look at the tests/ directory for working examples of every feature.
-->

## Quick Example

```
-- A complete milang program
Shape = {Circle radius; Rect width height}

area s = s ->
  Circle = 3.14 * s.radius * s.radius
  Rect = s.width * s.height

main world =
  world.io.println (area (Circle 5))
  world.io.println (area (Rect 3 4))
  0
```

## How It Compiles

```
milang source → parse → import resolution → partial evaluation → C codegen → gcc
```

The partial evaluator is the heart of milang: it reduces all compile-time-known expressions to values, leaving only runtime-dependent code in the output. This means zero runtime overhead for abstractions that are fully known at compile time.
