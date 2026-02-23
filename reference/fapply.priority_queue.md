# Apply a function over priority queue entries

Apply a function over priority queue entries

## Usage

``` r
# S3 method for class 'priority_queue'
fapply(X, FUN, ...)
```

## Arguments

- X:

  A \`priority_queue\`.

- FUN:

  Function of \`(item, priority, name, ...)\` returning the new payload
  item. Queue metadata (\`priority\`, \`name\`) is read-only.

- ...:

  Additional arguments passed to \`FUN\`.

## Value

A new \`priority_queue\` with transformed entries.
