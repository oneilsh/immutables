# Check whether a priority queue is empty

Check whether a priority queue is empty

## Usage

``` r
is_empty(q)
```

## Arguments

- q:

  A \`priority_queue\`.

## Value

Logical scalar.

## Examples

``` r
x <- priority_queue()
is_empty(x)
#> [1] TRUE

x2 <- priority_queue("a", priorities = 1)
is_empty(x2)
#> [1] FALSE
```
