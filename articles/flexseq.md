# Getting Started with flexseq

## Creating sequences

`flexseq` objects are persistent (immutable) sequences. Each update
returns a new object.

``` r
x <- flexseq(1, 2, 3)
x
#> Unnamed flexseq with 3 elements.
#> 
#> Elements:
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
#> Unnamed flexseq with 5 elements.
#> 
#> Elements:
#> 
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> ... (skipping 1 element)
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
#> Unnamed flexseq with 3 elements.
#> 
#> Elements:
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
#> Unnamed flexseq with 6 elements.
#> 
#> Elements:
#> 
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> ... (skipping 2 elements)
#> 
#> [[5]]
#> [1] "e"
#> 
#> [[6]]
#> [1] "f"

x5 <- push_front(x4, "start")
x5
#> Unnamed flexseq with 7 elements.
#> 
#> Elements:
#> 
#> [[1]]
#> [1] "start"
#> 
#> [[2]]
#> [1] "a"
#> 
#> ... (skipping 3 elements)
#> 
#> [[6]]
#> [1] "e"
#> 
#> [[7]]
#> [1] "f"
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
#> Unnamed flexseq with 6 elements.
#> 
#> Elements:
#> 
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> ... (skipping 2 elements)
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
#> Named flexseq with 3 elements.
#> 
#> Elements:
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
#> Unnamed flexseq with 6 elements.
#> 
#> Elements:
#> 
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> ... (skipping 2 elements)
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
#> Unnamed flexseq with 5 elements.
#> 
#> Elements:
#> 
#> [[1]]
#> [1] 10
#> 
#> [[2]]
#> [1] 20
#> 
#> ... (skipping 1 element)
#> 
#> [[4]]
#> [1] 40
#> 
#> [[5]]
#> [1] 50
```
