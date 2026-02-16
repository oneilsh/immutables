# Build a Priority Queue from elements and priorities

Build a Priority Queue from elements and priorities

## Usage

``` r
as_priority_queue(x, priorities, names = NULL, monoids = NULL)
```

## Arguments

- x:

  Elements to enqueue.

- priorities:

  Numeric priorities (same length as \`x\`).

- names:

  Optional element names for name-based indexing.

- monoids:

  Optional additional named list of \`measure_monoid\` objects.

## Value

A \`priority_queue\`.

## Examples

``` r
x <- as_priority_queue(letters[1:4], priorities = c(3, 1, 2, 1))
x
#> <priority_queue> size=4 next_seq=5
#> min_priority=1 max_priority=3
peek_min(x)
#> [1] "b"
peek_max(x)
#> [1] "a"
```
