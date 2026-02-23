# Insert elements at a position

Inserts one or more elements before the current element at \`index\`.

## Usage

``` r
insert_at(x, index, values)
```

## Arguments

- x:

  A \`flexseq\`.

- index:

  One-based insertion position in \`\[1, length(x) + 1\]\`.

- values:

  Elements to insert. Supports scalar/vector/list/flexseq inputs.

## Value

Updated sequence with inserted elements.

## Examples

``` r
s <- as_flexseq(letters[1:4])
insert_at(s, 3, c("x", "y"))
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
#> [1] "x"
#> 
#> [[4]]
#> [1] "y"
#> 
#> [[5]]
#> [1] "c"
#> 
#> [[6]]
#> [1] "d"
#> 
```
