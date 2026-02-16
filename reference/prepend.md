# Prepend an element

Prepend an element

## Usage

``` r
prepend(t, x)
```

## Arguments

- t:

  A \`flexseq\`.

- x:

  Element to prepend.

## Value

Updated tree. If \`t\` is name-indexed, prepending unnamed elements is
invalid.

## Examples

``` r
x <- as_flexseq(letters[2:5])
x
#> FingerTree <size=4, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "b"
#> 
#> [[2]]
#> [1] "c"
#> 
#> [[3]]
#> [1] "d"
#> 
#> [[4]]
#> [1] "e"
#> 

x2 <- prepend(x, "a")
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
#> [1] "e"
#> 

# prepending to a named sequence requires a named element
x3 <- as_flexseq(list(two = 2, three = 3))
new_el <- 1
names(new_el) <- "one"
x4 <- prepend(x3, new_el)
x4
#> FingerTree <size=3, named=yes>
#>   monoids: none
#> 
#> $one
#> one 
#>   1 
#> 
#> $two
#> [1] 2
#> 
#> $three
#> [1] 3
#> 
```
