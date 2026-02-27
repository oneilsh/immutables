# Apply a function over interval index entries

Apply a function over interval index entries

## Usage

``` r
# S3 method for class 'interval_index'
fapply(X, FUN, ..., preserve_custom_monoids = TRUE)
```

## Arguments

- X:

  An \`interval_index\`.

- FUN:

  Function of \`(item, start, end, name, ...)\` returning the new
  payload item. Interval metadata (\`start\`, \`end\`, \`name\`) is
  read-only.

- ...:

  Additional arguments passed to \`FUN\`.

- preserve_custom_monoids:

  Logical scalar. If \`TRUE\` (default), preserve attached user monoids
  during rebuild. If \`FALSE\`, drop user monoids and keep only required
  interval/ordered/structural monoids.

## Value

A new \`interval_index\` with transformed entries.
