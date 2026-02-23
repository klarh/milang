# Documentation (`:?`)

The `:?` annotation domain attaches documentation to a binding. Doc expressions are ordinary milang expressions — usually strings or structured records — that the compiler stores as compile-time metadata. They do not affect runtime behavior.

## Simple String Docs

The most common form is a short description string:

```milang
add :? "Add two numbers"
add a b = a + b
```

## Structured Docs

For richer documentation, use a record with `fields` like `summary`, `params`, and `returns`:

```milang
distance :? {
  summary = "Squared distance between two points"
  params = {
    x1 = "First x coordinate"
    y1 = "First y coordinate"
    x2 = "Second x coordinate"
    y2 = "Second y coordinate"
  }
  returns = "The squared distance as a number"
}
distance x1 y1 x2 y2 = (x2 - x1)**2 + (y2 - y1)**2
```

The field names are not enforced — you can use whatever structure makes sense for your project.

## Triple-Quoted String Docs

For multi-line documentation, use triple-quoted strings. Margin stripping (based on the closing `"""` indentation) keeps the source tidy:

```milang
greet :? """
  Greet a person by name.
  Prints a friendly message to the console.
  """
greet world name = world.io.println ("Hello, " + name + "!")
```

## Example

```milang,run
add :? "Add two numbers"
add :: Num : Num : Num
add a b = a + b

distance :? {summary = "Squared distance"; returns = "Num"}
distance x1 y1 x2 y2 = (x2 - x1)**2 + (y2 - y1)**2

main world =
  world.io.println (add 3 4)
  world.io.println (distance 0 0 3 4)
```

Doc annotations do not change execution — the output above is the same with or without `:?` lines.

## Combining All Five Domains

Every annotation domain can coexist on a single binding:

```milang
distance :? "Squared Euclidean distance"
distance :: Num : Num : Num : Num : Num
distance :~ []
distance x1 y1 x2 y2 = (x2 - x1)**2 + (y2 - y1)**2
```

The domains are `=` (value), `::` (type), `:~` (traits), `:?` (docs), and `:!` (parse). They are independent and can appear in any order before the value binding.

## Future: `milang doc`

A planned `milang doc` command will extract `:?` annotations from source files and generate reference documentation automatically.
