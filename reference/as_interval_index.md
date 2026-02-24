# Build an Interval Index from elements and interval bounds

Build an Interval Index from elements and interval bounds

## Usage

``` r
as_interval_index(x, start, end, bounds = "[)")
```

## Arguments

- x:

  Elements to add.

- start:

  Scalar start endpoints (same length as \`x\`).

- end:

  Scalar end endpoints (same length as \`x\`).

- bounds:

  One of \`"\[)"\`, \`"\[\]"\`, \`"()"\`, \`"(\]"\`.

## Value

An \`interval_index\`.

## Examples

``` r
ix <- as_interval_index(c("a", "b", "c"), start = c(1, 2, 2), end = c(3, 2, 4))
ix
#> Unnamed interval_index with 3 elements, default bounds [start, end).
#> 
#> Elements (by interval start order):
#> 
#> [[1]] (interval [1, 3))
#> [1] "a"
#> 
#> [[2]] (interval [2, 2))
#> [1] "b"
#> 
#> [[3]] (interval [2, 4))
#> [1] "c"
#> 
as.list(peek_point(ix, 2, which = "all"))
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "c"
#> 
```
