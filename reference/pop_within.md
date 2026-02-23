# Pop intervals within a query interval

Pop intervals within a query interval

## Usage

``` r
pop_within(x, start, end, which = c("first", "all"), bounds = NULL)
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

For \`which = "first"\`, a list with \`element\`, \`start\`, \`end\`,
\`remaining\`. For \`which = "all"\`, \`element\` is an
\`interval_index\` slice and \`start\`/\`end\` are \`NULL\`.
