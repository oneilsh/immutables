# Construct an Ordered Sequence

Construct an Ordered Sequence

## Usage

``` r
ordered_sequence(..., keys = NULL, monoids = NULL)
```

## Arguments

- ...:

  Elements to add.

- keys:

  Scalar key values matching \`...\` length.

- monoids:

  Optional additional named list of \`measure_monoid\` objects.

## Value

An \`ordered_sequence\`.

## Examples

``` r
xs <- ordered_sequence("bb", "a", "ccc", keys = c(2, 1, 3))
xs
#> <ordered_sequence> size=3 key_type=numeric
#>   preview[3]: {key= num 1 item= chr "a"} | {key= num 2 item= chr "bb"} | {key= num 3 item= chr "ccc"}
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
