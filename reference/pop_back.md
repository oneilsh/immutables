# Pop the back element

Returns both the popped element and the remaining sequence.

## Usage

``` r
pop_back(x)
```

## Arguments

- x:

  A \`flexseq\`.

## Value

A list with fields \`element\` and \`remaining\`.

## Examples

``` r
s <- as_flexseq(letters[1:3])
out <- pop_back(s)
out$element
#> [1] "c"
out$remaining
#> FingerTree <size=2, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
```
