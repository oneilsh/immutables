# Find first element with key \>= value

Find first element with key \>= value

## Usage

``` r
lower_bound(x, key)
```

## Arguments

- x:

  An \`ordered_sequence\`.

- key:

  Query key.

## Value

Named list with fields \`found\`, \`index\`, \`element\`, and \`key\`.
When no match exists, \`found\` is \`FALSE\` and the remaining fields
are \`NULL\`.
