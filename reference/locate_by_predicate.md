# Locate First Predicate Flip Without Reconstructing Context Trees

Read-only analogue of \[split_around_by_predicate()\]: finds the
distinguished element where the scan predicate flips, but does not
rebuild left/right trees.

## Usage

``` r
locate_by_predicate(
  t,
  predicate,
  monoid_name,
  accumulator = NULL,
  include_metadata = FALSE
)
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

- include_metadata:

  Logical; include left/hit/right measures and index.

## Value

If \`include_metadata = FALSE\`: \`list(found, elem)\`. If \`TRUE\`:
\`list(found, elem, metadata = list(left_measure, hit_measure,
right_measure, index))\`.

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

loc <- locate_by_predicate(x, function(v) v >= 4, ".size")
loc
#> $found
#> [1] TRUE
#> 
#> $elem
#> [1] "d"
#> 

# include metadata with a custom monoid
sum_m <- measure_monoid(`+`, 0, as.numeric)
x2 <- as_flexseq(1:6, monoids = list(sum = sum_m))
loc2 <- locate_by_predicate(x2, function(v) v >= 10, "sum", include_metadata = TRUE)
loc2
#> $found
#> [1] TRUE
#> 
#> $elem
#> [1] 4
#> 
#> $metadata
#> $metadata$left_measure
#> [1] 6
#> 
#> $metadata$hit_measure
#> [1] 10
#> 
#> $metadata$right_measure
#> [1] 11
#> 
#> $metadata$index
#> [1] 4
#> 
#> 
```
