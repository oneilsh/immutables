# Apply a function over flexseq elements

Apply a function over flexseq elements

## Usage

``` r
# S3 method for class 'flexseq'
fapply(X, FUN, ..., preserve_custom_monoids = TRUE)
```

## Arguments

- X:

  A \`flexseq\`.

- FUN:

  Function to apply to each element.

- ...:

  Additional arguments passed to \`FUN\`.

- preserve_custom_monoids:

  Logical scalar. If \`TRUE\` (default), rebuild with the full current
  monoid set (including user monoids). If \`FALSE\`, rebuild using only
  required structural monoids (\`.size\`, \`.named_count\`).

## Value

A new \`flexseq\` with transformed elements.
