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
#> 

s <- split_around_by_predicate(x, function(v) v >= 4, ".size")
s$elem
#> [1] "d"
s$left
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
s$right
#> Unnamed flexseq with 2 elements.
#> 
#> Elements:
#> 
#> [[1]]
#> [1] "e"
#> 
#> [[2]]
#> [1] "f"
#> 
```
