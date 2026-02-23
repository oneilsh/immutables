# Apply a function over ordered sequence entries

Apply a function over ordered sequence entries

## Usage

``` r
# S3 method for class 'ordered_sequence'
fapply(X, FUN, ...)
```

## Arguments

- X:

  An \`ordered_sequence\`.

- FUN:

  Function of \`(item, key, name, ...)\` returning the new payload item.
  Key metadata (\`key\`, \`name\`) is read-only.

- ...:

  Additional arguments passed to \`FUN\`.

## Value

A new \`ordered_sequence\` with transformed entries.
