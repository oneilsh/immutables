# Construct a Measure Monoid Specification

Construct a Measure Monoid Specification

## Usage

``` r
measure_monoid(f, i, measure)
```

## Arguments

- f:

  Associative binary function over measure values.

- i:

  Identity measure value for \`f\`.

- measure:

  Function mapping a raw element to a measure value.

## Value

An object of class \`measure_monoid\`.

## Examples

``` r
sum_m <- measure_monoid(`+`, 0, as.numeric)
x <- as_flexseq(1:5)
fold_left(x, sum_m)
#> [1] 15

# create a second monoid and add it to a sequence
nchar_sum <- measure_monoid(`+`, 0, function(el) nchar(as.character(el)))
x2 <- as_flexseq(letters[1:3])
x3 <- add_monoids(x2, list(nchar_sum = nchar_sum))
attr(x3, "measures")$nchar_sum
#> [1] 3
```
