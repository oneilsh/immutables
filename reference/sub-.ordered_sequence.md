# Index an ordered sequence

Ordered sequences support read indexing while preserving key-order
semantics.

## Usage

``` r
# S3 method for class 'ordered_sequence'
x[i, ...]

# S3 method for class 'ordered_sequence'
x[[i, ...]]

# S3 method for class 'ordered_sequence'
x[i] <- value

# S3 method for class 'ordered_sequence'
x[[i]] <- value
```

## Arguments

- x:

  An \`ordered_sequence\`.

- i:

  For \`\[\`, positive integer indices, character names, or logical
  mask.

- ...:

  Unused.

- value:

  Replacement value (unsupported).

## Value

For \`\[\`, an \`ordered_sequence\` subset that preserves key order.

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

\[\\.flexseq\], \[\\\<-.ordered_sequence\], \[\\\\\<-.ordered_sequence\]

## Examples

``` r
x <- as_ordered_sequence(list("b", "a", "c"), keys = c(2, 1, 3))
x[1:2]
#> Unnamed ordered_sequence with 2 elements.
#> 
#> Elements (by key order):
#> 
#> [[1]] (key 1)
#> [1] "a"
#> 
#> [[2]] (key 2)
#> [1] "b"
#> 
try(x[c(2, 1)])
#> Error in .ord_assert_positions_strict(idx) : 
#>   Ordered subsetting requires strictly increasing indices (no duplicates or reordering).
```
