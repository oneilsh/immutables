# Pop an element by position

Returns both the popped element and the remaining sequence.

## Usage

``` r
pop_at(x, index)
```

## Arguments

- x:

  A \`flexseq\`.

- index:

  One-based position to remove.

## Value

A list with fields \`element\` and \`remaining\`.

## Examples

``` r
s <- as_flexseq(letters[1:4])
out <- pop_at(s, 3)
out$element
#> [1] "c"
out$remaining
#> FingerTree <size=3, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> [[3]]
#> [1] "d"
#> 
```
