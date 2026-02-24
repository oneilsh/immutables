# Construct an Ordered Sequence

Construct an Ordered Sequence

## Usage

``` r
ordered_sequence(..., keys)
```

## Arguments

- ...:

  Elements to add.

- keys:

  Scalar key values matching \`...\` length.

## Value

An \`ordered_sequence\`.

## Examples

``` r
xs <- ordered_sequence("bb", "a", "ccc", keys = c(2, 1, 3))
xs
#> Unnamed ordered_sequence with 3 elements.
#> 
#> Elements (by key order):
#> 
#> [[1]] (key 1)
#> [1] "a"
#> 
#> [[2]] (key 2)
#> [1] "bb"
#> 
#> [[3]] (key 3)
#> [1] "ccc"
#> 
lower_bound(xs, 2)
#> $found
#> [1] TRUE
#> 
#> $index
#> [1] 2
#> 
#> $element
#> [1] "bb"
#> 
#> $key
#> [1] 2
#> 
```
