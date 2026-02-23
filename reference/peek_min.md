# Peek minimum-priority element

Peek minimum-priority element

## Usage

``` r
peek_min(q)
```

## Arguments

- q:

  A \`priority_queue\`.

## Value

Element with minimum priority (stable on ties).

## Examples

``` r
x <- priority_queue("a", "b", "c", priorities = c(2, 1, 1))
x
#> <priority_queue> size=3
#> min_priority=1 max_priority=2
peek_min(x)
#> [1] "b"
```
