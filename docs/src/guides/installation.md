# Installation

Milang is built from source using the Haskell toolchain and compiles programs to C via `gcc`.

## Pre-built Binaries

Automated builds for Windows, macOS, and Linux (static) are available at:
**<https://github.com/klarh/milang/actions/workflows/build.yml>**

Download the artifact for your platform from any successful workflow run — no Haskell toolchain needed.

## Prerequisites

You need three things installed:

| Tool | Minimum version | Purpose |
|------|----------------|---------|
| **GHC** | 9.6+ | Haskell compiler (builds the milang compiler itself) |
| **cabal** | 3.10+ | Haskell build tool |
| **gcc** | any recent | C compiler (milang emits C, then calls gcc to produce binaries) |

### Ubuntu / Debian

```bash
sudo apt install ghc cabal-install build-essential
```

### Arch Linux

```bash
sudo pacman -S ghc cabal-install base-devel
```

### macOS (Homebrew)

```bash
brew install ghc cabal-install gcc
```

## Building from Source

Clone the repository and build:

```bash
git clone <repository>
cd milang
make
```

`make` runs `cabal build` inside the `core/` directory.

If you prefer to do it manually:

```bash
cd core
cabal update
cabal build
```

For a statically linked compiler build (Linux) using Podman, you can use the provided Makefile in the core/ directory:

```bash
cd core
make -f Makefile.static
# now ./milang is available
```

## Verifying the Installation

Start the REPL to confirm everything works:

```bash
./milang repl
```

You should see a `λ>` prompt. Try evaluating an expression:

```text
λ> 2 + 3
5
```

Press Ctrl-D to exit.

Run the test suite to make sure the compiler is healthy:

```bash
make test
```

This compiles and runs every `.mi` file in the repository's test suite. A successful run prints something like `Passed: 60, Failed: 0`.
