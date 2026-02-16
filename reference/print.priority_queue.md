# Print a Priority Queue

Print a Priority Queue

## Usage

``` r
# S3 method for class 'priority_queue'
print(x, ...)
```

## Arguments

- x:

  A \`priority_queue\`.

- ...:

  Unused.

## Value

\`x\` invisibly.

## Examples

``` r
x <- priority_queue("a", "b", "c", priorities = c(2, 1, 3))
x
#> <priority_queue> size=3 next_seq=4
#> min_priority=1 max_priority=3
```
