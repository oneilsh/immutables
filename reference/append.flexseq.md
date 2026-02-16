# Append an element to a flexseq

Append an element to a flexseq

## Usage

``` r
# S3 method for class 'flexseq'
append(x, values, ...)
```

## Arguments

- x:

  A \`flexseq\`.

- values:

  Element to append.

- ...:

  Unused.

## Value

Updated \`flexseq\`.

## Examples

``` r
x <- as_flexseq(letters[1:4])
x
#> FingerTree <size=4, named=no>
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
#> [[4]]
#> [1] "d"
#> 

x2 <- append(x, "p")
x2
#> FingerTree <size=5, named=no>
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
#> [[4]]
#> [1] "d"
#> 
#> [[5]]
#> [1] "p"
#> 

# flexseqs can also hold nested or mixed types
x3 <- append(x2, c(8, 9))
x3
#> FingerTree <size=6, named=no>
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
#> [[4]]
#> [1] "d"
#> 
#> [[5]]
#> [1] "p"
#> 
#> [[6]]
#> [1] 8 9
#> 

# appending to a named sequence requires a named element
x4 <- as_flexseq(list(one = 1, two = 2, three = 3))
new_el <- 4
names(new_el) <- "four"
x5 <- append(x4, new_el)
x5
#> FingerTree <size=4, named=yes>
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
#> $four
#> four 
#>    4 
#> 
```
