# Push an element to the front

Push an element to the front

## Usage

``` r
push_front(x, value)
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
s <- as_flexseq(letters[2:4])
s2 <- push_front(s, "a")
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
new_el <- 1
names(new_el) <- "one"
push_front(n, new_el)
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
