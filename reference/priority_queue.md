# Construct a Priority Queue

Priority queues expose queue-oriented operations (\`insert\`,
\`peek\_\*\`, \`pop\_\*\`, and \`fapply\`). For full sequence-style
editing and traversal, cast explicitly with \`as_flexseq()\`.

## Usage

``` r
priority_queue(..., priorities)
```

## Arguments

- ...:

  Elements to enqueue.

- priorities:

  Scalar non-missing orderable priorities matching \`...\`.

## Value

A \`priority_queue\`.

## Examples

``` r
x <- priority_queue("a", "b", "c", priorities = c(2, 1, 2))
x
#> Unnamed priority_queue with 3 elements.
#> Minimum priority: 1, Maximum priority: 2
#> 
#> Elements (by priority):
#> 
#> (priority 1)
#> [1] "b"
#> 
#> (priority 2)
#> [1] "a"
#> 
#> (priority 2)
#> [1] "c"
#> 
peek_min(x)
#> [1] "b"
```
