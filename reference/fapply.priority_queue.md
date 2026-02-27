# Apply a function over priority queue entries

Apply a function over priority queue entries

## Usage

``` r
# S3 method for class 'priority_queue'
fapply(X, FUN, ..., preserve_custom_monoids = TRUE)
```

## Arguments

- X:

  A \`priority_queue\`.

- FUN:

  Function of \`(item, priority, name, ...)\` returning the new payload
  item. Queue metadata (\`priority\`, \`name\`) is read-only.

- ...:

  Additional arguments passed to \`FUN\`.

- preserve_custom_monoids:

  Logical scalar. If \`TRUE\` (default), preserve attached user monoids
  during rebuild. If \`FALSE\`, drop user monoids and keep only required
  queue/structural monoids.

## Value

A new \`priority_queue\` with transformed entries.
