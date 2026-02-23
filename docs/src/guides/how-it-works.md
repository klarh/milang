# How Milang Works

Milang's compilation pipeline has four stages:

```bash
source.mi -> Parser -> Import Resolution -> Partial Evaluator -> C Codegen -> gcc
```

Each stage is a pure transformation of the AST, except for import resolution (which reads files and URLs) and the final gcc invocation.

## 1. Parser

The parser is indentation-sensitive — nested blocks are determined by whitespace, similar to Haskell or Python.

There are **zero keywords** in milang. Everything that looks like a keyword — `if`, `import`, `true`, `false` — is actually a function or value defined in the prelude. The parser only needs to recognize:

- **Bindings** across five annotation domains: `=` (value), `::` (type), `:~` (traits), `:?` (docs), `:!` (parse)
- **Expressions**: literals, application, operators, lambdas, records, lists, pattern match (`->`)
- **Operators**: parsed with configurable precedence (`:!` declarations can define new ones)

The output is a single `Expr` AST type with variants like `IntLit`, `App`, `Lam`, `Namespace`, `Record`, `Match`, and so on.

## 2. Import Resolution

When the parser encounters `import "path.mi"`, the import resolver:

1. **Reads the file** (local path or URL)
2. **Parses it** into an AST
3. **Recursively resolves** its imports
4. **Returns a record** of the module's exported bindings

Import types:

| Syntax | Source |
|--------|--------|
| `import "lib/utils.mi"` | Local file (relative to importing file) |
| `import "https://example.com/lib.mi"` | URL (downloaded and cached) |
| `import "/usr/include/math.h"` | C header (extracts function signatures for FFI) |

**URL security:** URL imports must be pinned with a SHA-256 hash using `import'` and a hash record. The `milang pin` command fetches imports and writes the hashes back into your source file. The hash covers the content of the import and all of its transitive sub-imports (a Merkle hash), so any tampering is detected.

**Circular imports** are handled by returning only the non-import bindings from the cycle and marking the recursive reference as a lazy thunk.

## 3. Partial Evaluator

The partial evaluator is the heart of the compiler. It walks the AST and **reduces every expression it can** given the values it knows at compile time.

Consider:

```milang,run
double x = x * 2
y = double 21
```

The partial evaluator sees that `double` is fully known and `21` is a literal, so it evaluates `double 21` at compile time. The result in the reduced AST is simply `y = 42` — the function call has been eliminated entirely.

Key techniques:

- **Strongly Connected Component (SCC) analysis** — bindings are sorted by dependency so each group can be reduced in order.
- **Depth-limited recursion** — recursive functions are unrolled a fixed number of times. If the result converges (reaches a base case), it becomes a compile-time constant. Otherwise, the function is left as runtime code.
- **Environment threading** — the evaluator carries a map of known bindings. When a binding's value is fully determined, it's substituted into all uses.

The partial evaluator **is** the optimizer. There is no separate optimization pass. Any abstraction that is fully known at compile time — constants, configuration, helper functions applied to literals, record construction — is resolved to a value before code generation.

## 4. C Code Generation

The code generator takes the reduced AST and emits a single, self-contained C file. This file includes:

- **An embedded runtime** — an arena allocator, a tagged union value type (`MiVal`), environment chains, and built-in functions.
- **Arena allocation** — all milang values are allocated from 1 MB arena blocks with 8-byte alignment. There is no garbage collector; arenas are freed in bulk.
- **Tagged unions** — every runtime value is a `MiVal` with a `tag` (`MI_INT`, `MI_FLOAT`, `MI_STRING`, `MI_CLOSURE`, `MI_RECORD`, etc.) and a payload.
- **Tail-call optimization** — `tail` calls are compiled to `goto` jumps, so recursive functions run in constant stack space.
- **Closures** — functions that capture variables are represented as a code pointer plus an environment chain of bindings.

The generated C compiles with `gcc` (or `clang`) and links against the standard C library:

```bash
gcc output.c -o program
```

## The Key Insight

Because the partial evaluator runs at compile time, **high-level abstractions often have zero runtime cost**. A chain of helper functions, a configuration record, a computed lookup table — if the inputs are known at compile time, none of that code exists in the generated binary. Only expressions that depend on runtime values (IO, user input, command-line arguments) survive into the emitted C.

## Debugging the Pipeline

Two compiler commands let you inspect intermediate stages:

- **`milang dump file.mi`** — shows the parsed AST before import resolution. Useful for checking how the parser interpreted your syntax.
- **`milang reduce file.mi`** — shows the AST after partial evaluation. This is what the code generator sees. Use it to verify that compile-time computation happened as expected.

```bash
./milang dump myfile.mi    # parsed AST
./milang reduce myfile.mi  # after partial evaluation
```
