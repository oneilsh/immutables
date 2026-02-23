# Index an interval index

Interval indexes support read indexing while preserving interval-order
semantics.

## Usage

``` r
# S3 method for class 'interval_index'
x$name

# S3 method for class 'interval_index'
x$name <- value

# S3 method for class 'interval_index'
x[i, ...]

# S3 method for class 'interval_index'
x[[i, ...]]

# S3 method for class 'interval_index'
x[i] <- value

# S3 method for class 'interval_index'
x[[i]] <- value
```

## Arguments

- x:

  An \`interval_index\`.

- name:

  Element name (for \`\$\` and \`\$\<-\`).

- value:

  Replacement value (unsupported).

- i:

  For \`\[\`, positive integer indices, character names, or logical
  mask.

- ...:

  Unused.

## Value

For \`\$\`: the matched payload element.

No return value; always errors because replacement indexing is
unsupported.

For \`\[\`, an \`interval_index\` subset that preserves interval order.

For \`\[\[\`, one payload element by scalar integer position or scalar
character name.

No return value; always errors because replacement indexing is
unsupported.

No return value; always errors because replacement indexing is
unsupported.

## Details

For \`\[\`: - integer and logical indices must resolve to strictly
increasing positions; - character indices are resolved by names and must
also be strictly increasing; - duplicates and reordering are rejected.

For \`\[\[\`: - accepts scalar integer position or scalar character name
and returns the payload element.

Replacement indexing (\`\[\<-\`, \`\[\[\<-\`) is intentionally
unsupported.

## See also

\[\\.flexseq\], \[\\\<-.interval_index\], \[\\\\\<-.interval_index\]

## Examples

``` r
x <- as_interval_index(list("a", "b", "c"), start = c(1, 2, 3), end = c(2, 4, 5))
x[1:2]
#> <interval_index> size=2 endpoint_type=numeric bounds=[)
#>   preview[2]: {start= num 1 end= num 2 item= chr "a"} | {start= num 2 end= num 4 item= chr "b"}
try(x[c(2, 1)])
#> Error in .ord_assert_positions_strict(idx) : 
#>   Ordered subsetting requires strictly increasing indices (no duplicates or reordering).
```
