# Parse Declarations (`:!`)

The `:!` annotation domain declares how the parser should handle a user-defined operator — specifically its precedence and associativity. The parser pre-scans the source for `:!` declarations before parsing expressions, so they take effect immediately.

## Syntax

```milang
(op) :! {prec = N; assoc = Left}
```

- **`prec`** — an integer precedence level. Higher values bind more tightly.
- **`assoc`** — one of `Left`, `Right`, or `None`. Determines how chains of the same operator group:
  - `Left`: `a op b op c` parses as `(a op b) op c`
  - `Right`: `a op b op c` parses as `a op (b op c)`
  - `None`: chaining is a parse error; explicit parentheses are required.

## Example

```milang,run
(<=>) :! {prec = 30; assoc = Left}
(<=>) a b = if (a == b) ~0 ~(if (a > b) ~1 ~(0 - 1))

cmp1 = 5 <=> 3
cmp2 = 3 <=> 3
cmp3 = 1 <=> 5
```

## Built-in Operator Precedences

For reference, the approximate precedence levels of built-in operators:

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 1 | `\|>` `>>` `<<` | Left |
| 2 | `\|\|` | Left |
| 3 | `&&` | Left |
| 4 | `==` `/=` `<` `>` `<=` `>=` | Left |
| 5 | `:` | Right |
| 6 | `+` `-` `++` | Left |
| 7 | `*` `/` `%` | Left |
| 8 | `**` | Right |

User-defined operators without a `:!` declaration receive a default precedence. Define `:!` to override this and integrate your operator naturally with built-in ones.

## Metaprogramming Hook

Because `:!` declarations are processed during parsing (before evaluation), they serve as a metaprogramming hook — they let you reshape how the parser reads subsequent expressions. Combined with user-defined operators, this gives you control over the syntactic structure of your programs.
