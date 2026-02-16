# Split Around First Predicate Flip

Splits a sequence into left context, matched element, and right context
at the first point where \`predicate\` becomes \`TRUE\` on accumulated
monoid measures.

## Usage

``` r
split_around_by_predicate(t, predicate, monoid_name, accumulator = NULL)
```

## Arguments

- t:

  A \`flexseq\`.

- predicate:

  Function on accumulated measure values.

- monoid_name:

  Name of monoid from \`attr(t, "monoids")\`.

- accumulator:

  Optional starting measure (defaults to monoid identity).

## Value

A list with \`left\`, \`elem\`, and \`right\`.

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

s <- split_around_by_predicate(x, function(v) v >= 4, ".size")
s$elem
#> [1] "d"
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
#> FingerTree <size=2, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "e"
#> 
#> [[2]]
#> [1] "f"
#> 
```
