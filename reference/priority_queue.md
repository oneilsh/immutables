# Construct a Priority Queue

Priority queues expose queue-oriented operations (\`insert\`,
\`peek\_\*\`, \`pop\_\*\`, and \`fapply\`). For full sequence-style
editing and traversal, cast explicitly with \`as_flexseq()\`.

## Usage

``` r
priority_queue(..., priorities = NULL, names = NULL, monoids = NULL)
```

## Arguments

- ...:

  Elements to enqueue.

- priorities:

  Numeric priorities matching \`...\`.

- names:

  Optional element names.

- monoids:

  Optional additional named list of \`measure_monoid\` objects.

## Value

A \`priority_queue\`.

## Examples

``` r
x <- priority_queue("a", "b", "c", priorities = c(2, 1, 2))
x
#> <priority_queue> size=3
#> min_priority=1 max_priority=2
peek_min(x)
#> [1] "b"
```
