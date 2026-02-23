# The Milang Programming Language

Milang is a minimalist functional programming language with:

- **Zero keywords** — everything is a function or operator, including `if`
- **Haskell-like syntax** — clean, whitespace-sensitive, expression-oriented
- **Partial evaluation** as the core compilation model — the compiler reduces your program as far as possible at compile time, then emits C code for what remains
- **Capability-based IO** — side effects flow through an explicit `world` value
- **Five annotation domains** — types (`::`) , traits (`:~`), docs (`:?`), parse declarations (`:!`), and values (`=`)

Milang is designed around three guiding principles: extreme simplicity, aggressive compile-time evaluation, and explicit capabilities for side effects.

There are no special syntactic forms in Milang — control flow, conditionals, and data construction are expressed with ordinary functions and operators. This uniform surface makes programs concise and composable, and helps both authors and tooling reason about code consistently.

Partial evaluation is the heart of the compiler: any expression that can be resolved at compile time is evaluated by the compiler itself. The result is that high-level abstractions often carry zero runtime cost — configuration, macro-like computation, and many optimizations happen automatically while compiling into straightforward C.

Milang uses an explicit capability-based IO model: the program entry point receives a `world` record that contains sub-records like `io` and `process`. By passing only the capabilities a function needs, you restrict what it can do. The compiler targets C and emits code suitable for `gcc` or `clang`, which makes Milang programs portable and fast.

The repository contains focused examples covering language features referenced throughout this guide.

## Quick Example

```milang,run
Shape = {Circle radius; Rect width height}

area s = s ->
  Circle = 3.14 * s.radius * s.radius
  Rect = s.width * s.height

main world =
  world.io.println (area (Circle 5))
  world.io.println (area (Rect 3 4))
```

## How It Compiles

```bash
milang source -> parse -> import resolution -> partial evaluation -> C codegen -> gcc
```

The partial evaluator is the heart of milang: it reduces all compile-time-known expressions to values, leaving only runtime-dependent code in the output. This means zero runtime overhead for abstractions that are fully known at compile time.
