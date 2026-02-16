# Split a flexseq into Left and Right Parts by Predicate

Splits a sequence at the point where the predicate first becomes TRUE on
accumulated monoid measures.

## Usage

``` r
split_by_predicate(x, predicate, monoid_name)
```

## Arguments

- x:

  A \`flexseq\`.

- predicate:

  Predicate function on accumulated measure values.

- monoid_name:

  Character scalar naming the monoid to scan.

## Value

A list with \`left\` and \`right\` flexseq objects.

## Examples

``` r
x <- as_flexseq(letters[1:6])
x
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
#> 

s <- split_by_predicate(x, function(v) v >= 4, ".size")
s$left
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
#> [1] "c"
#> 
s$right
#> FingerTree <size=3, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "d"
#> 
#> [[2]]
#> [1] "e"
#> 
#> [[3]]
#> [1] "f"
#> 
```
