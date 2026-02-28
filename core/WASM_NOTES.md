WASM build notes — milang-core

Goal

Build a browser-embeddable WASM target for a Milang evaluator derived from core/, using GHC's wasm backend where possible.

High-level approach

1. Build a small, browser-friendly entrypoint that parses Milang source, injects the prelude, and runs the unified reducer entirely in-memory (no gcc/codegen step). This is implemented as Core.WebREPL (evalProgram/evalExpr) and a small executable WebMain.hs that reads stdin and prints reduced AST.

2. Add a cabal executable target (milang-wasm) that can be compiled with GHC's wasm backend. Prefer compiling the interpreter/reducer rather than codegen-to-C + gcc in the browser.

Compatibility checklist (what must change to build to WASM)

- Remove or conditionally compile away any use of System.Process/System.Directory/System.FilePath/temporary/haskeline: these cannot run in the browser. Use CPP flag (-DWASM) for wasm builds and provide browser stubs or JS FFI where appropriate.
- Core.Remote.fetchRemote: replace curl/readProcess with a browser fetch implementation (JS FFI) or make fetchRemote return an error if imports are used (acceptable for MVP). Use Web Crypto (SubtleCrypto) or a pure Haskell SHA-256 implementation available in JS to compute hashes in-browser.
- Core.CHeader: clang/gcc-based preprocessing is not available in-browser; either precompute C header signatures at build time or provide a minimal set of builtin signatures and skip preprocessing.
- Core.Codegen/Main.run: codegen emits C and calls gcc; for the browser REPL, prefer interpreting/reducing and pretty-printing results rather than invoking the compiled C runtime.

Build steps (developer machine / CI)

1. Ensure GHC + cabal have a healthy package DB (run `ghc-pkg check` and `cabal update`).
2. Build the milang-wasm executable locally: `cd core && cabal v2-build exe:milang-wasm` (may need to pass extra ghc-options like `-DWASM` depending on the flags used).
3. If targeting the browser via GHC wasm backend, follow the GHC wasm backend docs to produce wasm + JS glue; alternatively use GHCJS/ghcjs-backend if preferred.

Next work items (can be automated)

- Add a cabal flag `wasm` and set `ghc-options: -DWASM` for the wasm exe; use CPP in Core.Remote/Core.CHeader to provide browser-friendly stubs.
- Implement JS FFI bindings for fetchRemote and a SHA-256 implementation (or use Web Crypto via JS FFI) and wire them under the `WASM` CPP guard.
- Add a small JS loader that instantiates the wasm module and exposes an `evalProgram(src)` function to web pages; wrap with a Web Worker for sandboxing.
- Create an embeddable HTML + JS bundle (docs/assets) and a docs/repl.html that uses the wasm module to run code.

Notes on current state

- Core.WebREPL and WebMain were added to provide a pure-evaluation entrypoint (no import resolution or filesystem fetch). A milang-wasm stanza was appended to core/milang-core.cabal.
- Attempted `cabal v2-build exe:milang-wasm` here but the environment showed GHC package DB inconsistencies; run `cabal update` / `ghc-pkg check` on your machine/CI to proceed with compilation.
