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
#> Unnamed priority_queue with 3 elements.
#> Minimum priority: 1, Maximum priority: 2
#> 
#> Elements (by priority):
#> 
#> (priority 1)
#> [1] "b"
#> 
#> (priority 1)
#> [1] "c"
#> 
#> (priority 2)
#> [1] "a"
#> 
peek_min(x)
#> [1] "b"
```
