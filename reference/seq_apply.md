# Apply a Function to Each Sequence Element

Maps \`f\` over elements of a \`flexseq\` and returns a new \`flexseq\`.
Traversal is linear, and rebuild uses standard tree construction.

## Usage

``` r
seq_apply(x, f, preserve_monoids = FALSE, ...)
```

## Arguments

- x:

  A \`flexseq\`.

- f:

  Function applied to each element.

- preserve_monoids:

  Logical; when \`TRUE\`, carries all input monoids to the output. When
  \`FALSE\` (default), output keeps only invariant monoids (\`.size\`,
  \`.named_count\`).

- ...:

  Additional arguments passed to \`f\`.

## Value

A \`flexseq\` with transformed elements.

## Examples

``` r
x <- as_flexseq(1:5)
seq_apply(x, function(v) v * 10)
#> FingerTree <size=5, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] 10
#> 
#> [[2]]
#> [1] 20
#> 
#> [[3]]
#> [1] 30
#> 
#> [[4]]
#> [1] 40
#> 
#> [[5]]
#> [1] 50
#> 

xn <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
seq_apply(xn, function(v) v + 1)
#> FingerTree <size=3, named=yes>
#>   monoids: none
#> 
#> $a
#> [1] 2
#> 
#> $b
#> [1] 3
#> 
#> $c
#> [1] 4
#> 
```
