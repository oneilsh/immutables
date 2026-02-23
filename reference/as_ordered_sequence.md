# Build an Ordered Sequence from elements

Build an Ordered Sequence from elements

## Usage

``` r
as_ordered_sequence(x, keys = NULL, monoids = NULL)
```

## Arguments

- x:

  Elements to add.

- keys:

  Scalar key values matching \`x\` length.

- monoids:

  Optional additional named list of \`measure_monoid\` objects.

  Ordered sequences are always key-sorted. Subsetting with \`\[\` is
  intentionally constrained to strictly increasing mapped positions (no
  duplicates or reordering) so the result remains ordered.

## Value

An \`ordered_sequence\`.

## Examples

``` r
xs <- as_ordered_sequence(c(4, 1, 2, 1), keys = c(4, 1, 2, 1))
xs
#> <ordered_sequence> size=4 key_type=numeric
#>   preview[4]: {key= num 1 item= num 1} | {key= num 1 item= num 1} | {key= num 2 item= num 2} | {key= num 4 item= num 4}
length(elements_between(xs, 1, 1))
#> [1] 2
```
