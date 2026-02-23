# Push an element to the back

Push an element to the back

## Usage

``` r
push_back(x, value)
```

## Arguments

- x:

  A \`flexseq\`.

- value:

  Element to push.

## Value

Updated tree. If \`x\` is name-indexed, pushing unnamed elements is
invalid.

## Examples

``` r
s <- as_flexseq(letters[1:3])
s2 <- push_back(s, "d")
s2
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

n <- as_flexseq(list(two = 2, three = 3))
new_el <- 4
names(new_el) <- "four"
push_back(n, new_el)
#> FingerTree <size=3, named=yes>
#>   monoids: none
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
