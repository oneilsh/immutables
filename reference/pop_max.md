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
#> Unnamed priority_queue with 2 elements.
#> Minimum priority: 2, Maximum priority: 3
#> 
#> Elements (by priority):
#> 
#> (priority 2)
#> [1] "a"
#> 
#> (priority 3)
#> [1] "c"
#> 
```
