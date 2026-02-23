# Partial Evaluation

Partial evaluation is milang's core compilation model. There is no separate
optimisation pass — the compiler itself evaluates every expression whose inputs
are known at compile time and emits C code only for what remains. The result is
that high-level abstractions (helper functions, configuration records, computed
constants) often carry **zero runtime cost**.

## How it works

When the compiler processes a binding it walks the expression tree with a
recursive reducer (`reduceD`). At each node it checks whether the operands are
*concrete* — literal integers, floats, strings, lambdas, or records whose
`fields` are themselves concrete. If they are, the expression is evaluated
immediately and replaced by its result. If any operand is unknown (a function
parameter, an IO result, etc.) the expression is left as *residual code* for
the C back-end to emit.

```milang
-- Fully reduced at compile time:
x = 6 * 7              -- becomes: x = 42
f a = a * 2
y = f 21                -- becomes: y = 42

-- Stays as residual code (parameter unknown):
double a = a * 2        -- emitted as a C function
```

### SCC dependency analysis

Bindings are sorted into *strongly connected components* so that each group is
reduced in dependency order. Mutually-recursive bindings land in the same SCC
and are handled together.

### Depth-limited recursion

Recursive functions are unrolled only when every argument is concrete, and
reduction is capped at a fixed depth (128 steps). This prevents the compiler
from looping on unbounded recursion while still collapsing finite recursive
computations at compile time.

## Zero-cost abstractions

Because the reducer runs before code generation, any abstraction that is fully
known at compile time disappears entirely from the output:

```milang,run
-- Configuration record — reduced away at compile time
config = {width = 800; height = 600}
pixels = config.width * config.height
```

The binding `pixels` is reduced to the integer `480000` before any C code is
generated. No record allocation, no field lookup — just a constant.

## Inspecting the reducer output

milang ships two commands for inspecting what the compiler sees:

```bash
milang dump file.mi      -- parsed AST (before reduction)
milang reduce file.mi    -- AST after partial evaluation (what codegen sees)
```

Comparing the two on the same file shows exactly which expressions were
collapsed and which remain as residual code. This is the primary tool for
understanding compile-time behaviour.

## What stays as residual code

Anything that depends on a value unknown at compile time is left for the C
back-end:

```milang
main world =
  line = world.io.readLine    -- runtime IO — cannot reduce
  world.io.println line       -- emitted as C call
```

Function parameters, IO results, and any expression transitively depending on
them are residual. Everything else is reduced.

<!-- Sized types and partial evaluation

When the reducer encounters expressions involving sized numeric types the
following notes explain the current behaviour and constraints:

- The width argument to `Int'`, `UInt'`, or `Float'` must be a compile-time
  constant. The reducer requires concrete `n` values to reason about types;
  expressions like `Int' (4 * 8)` will reduce to `Int' 32` when the arithmetic
  inside the type expression is concrete.

- Type aliases such as `Int = Int' 64` are treated as syntactic sugar and are
  resolved during reduction; they do not remain at runtime.

- Current implementation note: sized types are primarily type-level and used
  for ABI/representation and annotation. The reducer evaluates constant
  arithmetic using Milang's general numeric semantics and does not automatically
  wrap or clamp values to a narrower machine width. If strict fixed-width
  arithmetic is required, use explicit conversion primitives or the C FFI,
  which maps milang sized types to fixed-width C types.

- Future work may include full width-aware reduction (clamping/wrapping at
  reduction time) and richer type-level computation.

-->

