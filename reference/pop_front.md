# Pop the front element

Returns both the popped element and the remaining sequence.

## Usage

``` r
pop_front(x)
```

## Arguments

- x:

  A \`flexseq\`.

## Value

A list with fields \`element\` and \`remaining\`.

## Examples

``` r
s <- as_flexseq(letters[1:3])
out <- pop_front(s)
out$element
#> [1] "a"
out$remaining
#> Unnamed flexseq with 2 elements.
#> 
#> Elements:
#> 
#> [[1]]
#> [1] "b"
#> 
#> [[2]]
#> [1] "c"
#> 
```
