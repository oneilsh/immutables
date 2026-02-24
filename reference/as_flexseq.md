# Coerce to a Persistent Flexible Sequence

For \`priority_queue\` inputs, this explicitly drops queue behavior and
returns a plain \`flexseq\` so full sequence operations are available.

## Usage

``` r
as_flexseq(x)
```

## Arguments

- x:

  Input vector/list-like object.

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
#> Unnamed flexseq with 5 elements.
#> 
#> Elements:
#> 
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> ... (skipping 1 element)
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 5
#> 

x2 <- as_flexseq(list(one = 1, two = 2, three = 3))
x2
#> Named flexseq with 3 elements.
#> 
#> Elements:
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
#> Unnamed flexseq with 2 elements.
#> 
#> Elements:
#> 
#> [[1]]
#> $item
#> [1] "a"
#> 
#> $priority
#> [1] 2
#> 
#> 
#> [[2]]
#> $item
#> [1] "b"
#> 
#> $priority
#> [1] 1
#> 
#> 
```
