# Extract maximum-priority element

Extract maximum-priority element

## Usage

``` r
extract_max(q)
```

## Arguments

- q:

  A \`priority_queue\`.

## Value

List with \`element\`, \`priority\`, and updated \`queue\`.

## Examples

``` r
x <- priority_queue("a", "b", "c", priorities = c(2, 3, 3))
out <- extract_max(x)
out$element
#> [1] "b"
out$priority
#> [1] 3
out$queue
#> <priority_queue> size=2 next_seq=4
#> min_priority=2 max_priority=3
```
