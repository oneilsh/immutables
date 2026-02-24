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

s <- split_by_predicate(x, function(v) v >= 4, ".size")
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
#> Unnamed flexseq with 3 elements.
#> 
#> Elements:
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
