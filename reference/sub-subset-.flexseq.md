# Replace one element by position or unique name

Replace one element by position or unique name

## Usage

``` r
# S3 method for class 'flexseq'
x[[i]] <- value
```

## Arguments

- x:

  A \`flexseq\`.

- i:

  Positive scalar integer index, or scalar character element name.

- value:

  Replacement element.

## Value

A new \`flexseq\` with one element replaced.

## Examples

``` r
x <- as_flexseq(letters[1:4])
x2 <- x
x2[[2]] <- "ZZ"
x2
#> FingerTree <size=4, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "ZZ"
#> 
#> [[3]]
#> [1] "c"
#> 
#> [[4]]
#> [1] "d"
#> 

x3 <- as_flexseq(setNames(as.list(1:3), c("x", "y", "z")))
x3[["y"]] <- 99
x3
#> FingerTree <size=3, named=yes>
#>   monoids: none
#> 
#> $x
#> [1] 1
#> 
#> $y
#> [1] 99
#> 
#> $z
#> [1] 3
#> 

# assigning NULL removes one element
x4 <- as_flexseq(letters[1:4])
x4[[2]] <- NULL
x4
#> FingerTree <size=3, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "c"
#> 
#> [[3]]
#> [1] "d"
#> 
```
