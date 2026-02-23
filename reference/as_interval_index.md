# Build an Interval Index from elements and interval bounds

Build an Interval Index from elements and interval bounds

## Usage

``` r
as_interval_index(x, start, end, bounds = "[)", monoids = NULL)
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

- monoids:

  Optional additional named list of \`measure_monoid\` objects.

## Value

An \`interval_index\`.

## Examples

``` r
ix <- as_interval_index(c("a", "b", "c"), start = c(1, 2, 2), end = c(3, 2, 4))
ix
#> <interval_index> size=3 endpoint_type=numeric bounds=[)
#>   preview[3]: {start= num 1 end= num 3 item= chr "a"} | {start= num 2 end= num 2 item= chr "b"} | {start= num 2 end= num 4 item= chr "c"}
as.list(peek_point(ix, 2, which = "all"))
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "c"
#> 
```
