# Pop maximum-priority element

Pop maximum-priority element

## Usage

``` r
pop_max(q)
```

## Arguments

- q:

  A \`priority_queue\`.

## Value

List with \`element\`, \`priority\`, and updated \`remaining\`.

## Examples

``` r
x <- priority_queue("a", "b", "c", priorities = c(2, 3, 3))
out <- pop_max(x)
out$element
#> [1] "b"
out$priority
#> [1] 3
out$remaining
#> <priority_queue> size=2
#> min_priority=2 max_priority=3
```
