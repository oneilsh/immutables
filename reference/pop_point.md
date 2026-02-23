# Pop intervals containing a point

Pop intervals containing a point

## Usage

``` r
pop_point(x, point, which = c("first", "all"), bounds = NULL)
```

## Arguments

- x:

  An \`interval_index\`.

- point:

  Query point.

- which:

  One of \`"first"\` or \`"all"\`.

- bounds:

  Optional boundary override. One of \`"\[)"\`, \`"\[\]"\`, \`"()"\`,
  \`"(\]"\`.

## Value

For \`which = "first"\`, a list with \`element\`, \`start\`, \`end\`,
\`remaining\`. For \`which = "all"\`, \`element\` is an
\`interval_index\` slice and \`start\`/\`end\` are \`NULL\`.
