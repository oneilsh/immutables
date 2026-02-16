# Replace selected elements by position or name

Replace selected elements by position or name

## Usage

``` r
# S3 method for class 'flexseq'
x[i] <- value
```

## Arguments

- x:

  A \`flexseq\`.

- i:

  Positive integer indices, character names, or logical mask.

- value:

  Replacement values; must have exactly same length as \`i\`.

## Value

A new \`flexseq\` with selected elements replaced.

## Examples

``` r
x <- as_flexseq(1:6)
x
#> FingerTree <size=6, named=no>
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
#> [[6]]
#> [1] 6
#> 

x2 <- x
x2[c(2, 5)] <- list(20, 50)
x2
#> FingerTree <size=6, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 20
#> 
#> [[3]]
#> [1] 3
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 50
#> 
#> [[6]]
#> [1] 6
#> 

# character replacement uses element names
x3 <- as_flexseq(setNames(as.list(1:4), c("a", "b", "c", "d")))
x3[c("d", "a")] <- list(40, 10)
x3
#> FingerTree <size=4, named=yes>
#>   monoids: none
#> 
#> $a
#> [1] 10
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 3
#> 
#> $d
#> [1] 40
#> 

# logical replacement supports recycling
x4 <- x
x4[c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)] <- list(1)
x4
#> FingerTree <size=6, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 1
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 1
#> 
#> [[6]]
#> [1] 6
#> 
```
