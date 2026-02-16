# Peek maximum-priority element

Peek maximum-priority element

## Usage

``` r
peek_max(q)
```

## Arguments

- q:

  A \`priority_queue\`.

## Value

Element with maximum priority (stable on ties).

## Examples

``` r
x <- priority_queue("a", "b", "c", priorities = c(2, 3, 3))
x
#> <priority_queue> size=3 next_seq=4
#> min_priority=2 max_priority=3
peek_max(x)
#> [1] "b"
```
