# Getting Started with flexseq

## Creating sequences

`flexseq` objects are persistent (immutable) sequences. Each update
returns a new object.

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

x2 <- as_flexseq(letters[1:5])
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
```

## Indexing

``` r
x2[[3]]
#> [1] "c"

x3 <- x2[c(1, 3, 5)]
x3
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
#> [1] "e"
```

## Pushing at either end

``` r
x4 <- push_back(x2, "f")
x4
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
#> [1] "e"
#> 
#> [[6]]
#> [1] "f"

x5 <- push_front(x4, "start")
x5
#> FingerTree <size=7, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "start"
#> 
#> [[2]]
#> [1] "a"
#> 
#> [[3]]
#> [1] "b"
#> 
#> [[4]]
#> [1] "c"
#> 
#> [[5]]
#> [1] "d"
#> 
#> [[6]]
#> [1] "e"
#> 
#> ... and 1 more element not shown
```

## Peeking and popping

``` r
peek_front(x5)
#> [1] "start"
peek_back(x5)
#> [1] "f"

pf <- pop_front(x5)
pf$element
#> [1] "start"
pf$remaining
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
#> [1] "e"
#> 
#> [[6]]
#> [1] "f"
```

## Named sequences

``` r
x_named <- as_flexseq(list(a = 1, b = 2, c = 3))
x_named
#> FingerTree <size=3, named=yes>
#>   monoids: none
#> 
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 3

x_named[["b"]]
#> [1] 2
x_named$c
#> [1] 3
```

## Concatenation

``` r
x6 <- as_flexseq(4:6)
x7 <- c(x, x6)
x7
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
```

## Applying a transform

``` r
x8 <- as_flexseq(1:5)
fapply(x8, function(el) el * 10)
#> FingerTree <size=5, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] 10
#> 
#> [[2]]
#> [1] 20
#> 
#> [[3]]
#> [1] 30
#> 
#> [[4]]
#> [1] 40
#> 
#> [[5]]
#> [1] 50
```
