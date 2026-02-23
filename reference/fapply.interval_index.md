# Apply a function over interval index entries

Apply a function over interval index entries

## Usage

``` r
# S3 method for class 'interval_index'
fapply(X, FUN, ...)
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

## Value

A new \`interval_index\` with transformed entries.
