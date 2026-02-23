# Peek intervals containing a point

Peek intervals containing a point

## Usage

``` r
peek_point(x, point, which = c("first", "all"), bounds = NULL)
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

For \`which = "first"\`, the payload item from the first match (or
\`NULL\` on no match). For \`which = "all"\`, an \`interval_index\`
slice of matches.
