# Coerce to a Persistent Flexible Sequence

For \`priority_queue\` inputs, this explicitly drops queue behavior and
returns a plain \`flexseq\` so full sequence operations are available.

## Usage

``` r
as_flexseq(x, monoids = NULL)
```

## Arguments

- x:

  Input vector/list-like object.

- monoids:

  Optional named list of measure monoids.

## Value

A \`flexseq\` object.

## Details

For \`ordered_sequence\` and \`interval_index\` inputs, this explicitly
drops ordered/interval behavior and returns a plain \`flexseq\` of
stored entries.

## Examples

``` r
x <- as_flexseq(1:5)
x
#> FingerTree <size=5, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 5
#> 

x2 <- as_flexseq(list(one = 1, two = 2, three = 3))
x2
#> FingerTree <size=3, named=yes>
#>   monoids: none
#> 
#> $one
#> [1] 1
#> 
#> $two
#> [1] 2
#> 
#> $three
#> [1] 3
#> 

q <- priority_queue("a", "b", priorities = c(2, 1))
as_flexseq(q)
#> FingerTree <size=2, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [[1]]$item
#> [1] "a"
#> 
#> [[1]]$priority
#> [1] 2
#> 
#> 
#> [[2]]
#> [[2]]$item
#> [1] "b"
#> 
#> [[2]]$priority
#> [1] 1
#> 
#> 
```
