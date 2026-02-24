# Construct an Interval Index

Construct an Interval Index

## Usage

``` r
interval_index(..., start, end, bounds = "[)")
```

## Arguments

- ...:

  Elements to add.

- start:

  Scalar start endpoints matching \`...\`.

- end:

  Scalar end endpoints matching \`...\`.

- bounds:

  One of \`"\[)"\`, \`"\[\]"\`, \`"()"\`, \`"(\]"\`.

## Value

An \`interval_index\`.

## Examples

``` r
ix <- interval_index("a", "b", "c", start = c(1, 2, 2), end = c(3, 2, 4))
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
```
