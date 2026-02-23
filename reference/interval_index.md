# Construct an Interval Index

Construct an Interval Index

## Usage

``` r
interval_index(..., start = NULL, end = NULL, bounds = "[)", monoids = NULL)
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

- monoids:

  Optional additional named list of \`measure_monoid\` objects.

## Value

An \`interval_index\`.

## Examples

``` r
ix <- interval_index("a", "b", "c", start = c(1, 2, 2), end = c(3, 2, 4))
ix
#> <interval_index> size=3 endpoint_type=numeric bounds=[)
#>   preview[3]: {start= num 1 end= num 3 item= chr "a"} | {start= num 2 end= num 2 item= chr "b"} | {start= num 2 end= num 4 item= chr "c"}
```
