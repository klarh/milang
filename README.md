# Milang

**Milang** is a minimalist functional programming language with zero keywords. It compiles to C via partial evaluation, targeting `gcc` or `clang`.

```milang
Shape = {Circle radius; Rect width height}

area s = s ->
  Circle = 3.14 * s.radius * s.radius
  Rect = s.width * s.height

main world =
  world.io.println (area (Circle 5))
  world.io.println (area (Rect 3 4))
```

## Highlights

- **Low keywords** — `if` is an ordinary prelude function, not special keyword syntax
- **Partial evaluation** — the compiler reduces everything known at compile time to literals; high-level abstractions can carry zero runtime cost
- **Capability-based IO** — side effects flow through an explicit `world` record; pass only the sub-record a function needs to restrict what it can do
- **Compiles to C** — portable, fast binaries; the emitted C embeds the runtime and needs only a standard C toolchain
- **Five annotation domains** for values — `=` (values), `::` (types), `:~` (traits/effects), `:?` (docs), `:!` (parsing rules)

## Documentation

Documentation source is available in the `docs/` subdirectory of the project.

| Format | Link |
|--------|------|
| Multi-page | [https://klarh.github.io/milang/html/](https://klarh.github.io/milang/html/) |
| Single page | [The Milang Programming Language](https://klarh.github.io/milang/pandoc/single/The-Milang-Programming-Language.html) |

## Quick Start

### Prerequisites

| Tool | Version | Purpose |
|------|---------|---------|
| GHC | 9.6+ | Builds the compiler |
| cabal | 3.10+ | Haskell build tool |
| gcc | any recent | Compiles emitted C |

**Ubuntu/Debian:** `sudo apt install ghc cabal-install build-essential`
**macOS:** `brew install ghc cabal-install gcc`

### Build

```bash
git clone https://github.com/klarh/milang.git
cd milang
make
```

### Run

```bash
./milang run hello.mi        # compile and run a .mi file
./milang repl                # interactive REPL
./milang compile hello.mi    # emit standalone C to hello.c
./milang reduce hello.mi     # show AST after partial evaluation
make test                    # run the full test suite
```

## A Taste of the Language

### Hello World

```milang
main world =
  world.io.println "Hello, Milang!"
```

### Pattern Matching & ADTs

```milang
List = {Nil; Cons head tail}

sum xs = xs ->
  Nil  = 0
  Cons = xs.head + sum xs.tail

result = sum [-1, 2, 3]
```

### Capability-Based IO

```milang
-- greet can only do IO, not access process or filesystem
greet io name = io.println ("Hello, " + name + "!")

main world =
  greet world.io "Alice"
```

### Custom Operators

```milang
-- declare operator precedence, then define it as a normal function
(|>) :! infixl 1
(|>) x f = f x

double x = 2*x

result = 13 |> double
```

### C FFI

```milang
math = import "/usr/include/math.h"

main world =
  world.io.println (toString (math.sqrt 2.0))
```

## Repository Layout

```
core/       Main compiler (Haskell, unified partial-evaluator architecture)
hs/         Reference implementation (separate passes; kept for comparison)
tests/      71 test programs covering language features
docs/       mdBook source for the documentation site
```

## License

See [LICENSE](LICENSE).
