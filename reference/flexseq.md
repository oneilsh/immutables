# Construct a Persistent Flexible Sequence

Works like \`list(...)\`, but returns an immutable sequence backed by
measured finger-tree internals.

## Usage

``` r
flexseq(...)
```

## Arguments

- ...:

  Sequence elements.

## Value

A \`flexseq\` object.

## Examples

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
#> 

x2 <- flexseq("a", "b", "c")
x2
#> Unnamed flexseq with 3 elements.
#> 
#> Elements:
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
