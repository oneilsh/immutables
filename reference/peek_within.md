# Peek intervals within a query interval

Peek intervals within a query interval

## Usage

``` r
peek_within(x, start, end, which = c("first", "all"), bounds = NULL)
```

## Arguments

- x:

  An \`interval_index\`.

- start:

  Query interval start.

- end:

  Query interval end.

- which:

  One of \`"first"\` or \`"all"\`.

- bounds:

  Optional boundary override. One of \`"\[)"\`, \`"\[\]"\`, \`"()"\`,
  \`"(\]"\`.

## Value

For \`which = "first"\`, the payload item from the first match (or
\`NULL\` on no match). For \`which = "all"\`, an \`interval_index\`
slice of matches.
