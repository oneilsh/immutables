# Extract minimum-priority element

Extract minimum-priority element

## Usage

``` r
extract_min(q)
```

## Arguments

- q:

  A \`priority_queue\`.

## Value

List with \`element\`, \`priority\`, and updated \`queue\`.

## Examples

``` r
x <- priority_queue("a", "b", "c", priorities = c(2, 1, 1))
out <- extract_min(x)
out$element
#> [1] "b"
out$priority
#> [1] 1
out$queue
#> <priority_queue> size=2 next_seq=4
#> min_priority=1 max_priority=2
```
