# Build a Priority Queue from elements and priorities

Build a Priority Queue from elements and priorities

## Usage

``` r
as_priority_queue(x, priorities)
```

## Arguments

- x:

  Elements to enqueue.

- priorities:

  Scalar non-missing orderable priorities (same length as \`x\`).

## Value

A \`priority_queue\`.

## Examples

``` r
x <- as_priority_queue(letters[1:4], priorities = c(3, 1, 2, 1))
x
#> Unnamed priority_queue with 4 elements.
#> Minimum priority: 1, Maximum priority: 3
#> 
#> Elements (by priority):
#> 
#> (priority 1)
#> [1] "b"
#> 
#> (priority 1)
#> [1] "d"
#> 
#> (priority 2)
#> [1] "c"
#> 
#> (priority 3)
#> [1] "a"
#> 
peek_min(x)
#> [1] "b"
peek_max(x)
#> [1] "a"
```
