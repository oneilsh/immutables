# Pop minimum-priority element

Pop minimum-priority element

## Usage

``` r
pop_min(q)
```

## Arguments

- q:

  A \`priority_queue\`.

## Value

List with \`element\`, \`priority\`, and updated \`remaining\`.

## Examples

``` r
x <- priority_queue("a", "b", "c", priorities = c(2, 1, 1))
out <- pop_min(x)
out$element
#> [1] "b"
out$priority
#> [1] 1
out$remaining
#> Unnamed priority_queue with 2 elements.
#> Minimum priority: 1, Maximum priority: 2
#> 
#> Elements (by priority):
#> 
#> (priority 1)
#> [1] "c"
#> 
#> (priority 2)
#> [1] "a"
#> 
```
