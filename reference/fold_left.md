# Fold Left Over a Sequence

Fold Left Over a Sequence

## Usage

``` r
fold_left(x, monoid)
```

## Arguments

- x:

  A \`flexseq\`.

- monoid:

  A \`measure_monoid\` specification.

## Value

Reduced value.

## Examples

``` r
x <- as_flexseq(1:5)
x
#> FingerTree <size=5, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 5
#> 

sum_m <- measure_monoid(`+`, 0, as.numeric)
fold_left(x, sum_m)
#> [1] 15

x2 <- as_flexseq(letters[1:4])
cat_m <- measure_monoid(paste0, "", as.character)
fold_left(x2, cat_m)
#> [1] "abcd"
```
