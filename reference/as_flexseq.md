# Coerce to a Persistent Flexible Sequence

Coerce to a Persistent Flexible Sequence

## Usage

``` r
as_flexseq(x, monoids = NULL)
```

## Arguments

- x:

  Input vector/list-like object.

- monoids:

  Optional named list of measure monoids.

## Value

A \`flexseq\` object.

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

x2 <- as_flexseq(list(one = 1, two = 2, three = 3))
x2
#> FingerTree <size=3, named=yes>
#>   monoids: none
#> 
#> $one
#> [1] 1
#> 
#> $two
#> [1] 2
#> 
#> $three
#> [1] 3
#> 
```
