# Build an Ordered Sequence from elements

Build an Ordered Sequence from elements

## Usage

``` r
as_ordered_sequence(x, keys)
```

## Arguments

- x:

  Elements to add.

- keys:

  Scalar key values matching \`x\` length.

  Ordered sequences are always key-sorted. Subsetting with \`\[\` is
  intentionally constrained to strictly increasing mapped positions (no
  duplicates or reordering) so the result remains ordered.

## Value

An \`ordered_sequence\`.

## Examples

``` r
xs <- as_ordered_sequence(c(4, 1, 2, 1), keys = c(4, 1, 2, 1))
xs
#> Unnamed ordered_sequence with 4 elements.
#> 
#> Elements (by key order):
#> 
#> [[1]] (key 1)
#> [1] 1
#> 
#> [[2]] (key 1)
#> [1] 1
#> 
#> [[3]] (key 2)
#> [1] 2
#> 
#> [[4]] (key 4)
#> [1] 4
#> 
length(elements_between(xs, 1, 1))
#> [1] 2
```
