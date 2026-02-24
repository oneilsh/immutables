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
#> Unnamed priority_queue with 3 elements.
#> Minimum priority: 2, Maximum priority: 3
#> 
#> Elements (by priority):
#> 
#> (priority 2)
#> [1] "a"
#> 
#> (priority 3)
#> [1] "b"
#> 
#> (priority 3)
#> [1] "c"
#> 
peek_max(x)
#> [1] "b"
```
