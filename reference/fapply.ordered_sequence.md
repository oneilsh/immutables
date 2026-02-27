# Apply a function over ordered sequence entries

Apply a function over ordered sequence entries

## Usage

``` r
# S3 method for class 'ordered_sequence'
fapply(X, FUN, ..., preserve_custom_monoids = TRUE)
```

## Arguments

- X:

  An \`ordered_sequence\`.

- FUN:

  Function of \`(item, key, name, ...)\` returning the new payload item.
  Key metadata (\`key\`, \`name\`) is read-only.

- ...:

  Additional arguments passed to \`FUN\`.

- preserve_custom_monoids:

  Logical scalar. If \`TRUE\` (default), preserve attached user monoids
  during rebuild. If \`FALSE\`, drop user monoids and keep only required
  ordered/structural monoids.

## Value

A new \`ordered_sequence\` with transformed entries.
