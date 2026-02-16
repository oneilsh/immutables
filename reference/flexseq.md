# Construct a Persistent Flexible Sequence

Works like \`list(...)\`, but returns an immutable sequence backed by
measured finger-tree internals.

## Usage

``` r
flexseq(..., monoids = NULL)
```

## Arguments

- ...:

  Sequence elements.

- monoids:

  Optional named list of measure monoids.

## Value

A \`flexseq\` object.

## Examples

``` r
x <- flexseq(1, 2, 3)
x
#> FingerTree <size=3, named=no>
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

x2 <- flexseq("a", "b", "c")
x2
#> FingerTree <size=3, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> [[3]]
#> [1] "c"
#> 
```
