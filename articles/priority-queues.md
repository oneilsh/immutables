# Priority Queues

## Creating a priority queue

``` r
x <- priority_queue("task_a", "task_b", "task_c", priorities = c(3, 1, 2))
x
#> <priority_queue> size=3
#> min_priority=1 max_priority=3
```

## Peeking at extremes

``` r
peek_min(x)
#> [1] "task_b"
peek_max(x)
#> [1] "task_a"
```

## Popping elements

[`pop_min()`](https://oneilsh.github.io/immutables/reference/pop_min.md)
and
[`pop_max()`](https://oneilsh.github.io/immutables/reference/pop_max.md)
return both the element and the updated queue.

``` r
x2 <- pop_min(x)
x2$element
#> [1] "task_b"
x2$priority
#> [1] 1
x2$remaining
#> <priority_queue> size=2
#> min_priority=2 max_priority=3
```

## Inserting elements

``` r
x3 <- insert(x, "task_d", priority = 0)
x3
#> <priority_queue> size=4
#> min_priority=0 max_priority=3
peek_min(x3)
#> [1] "task_d"
```

## Persistence

The original queue is unchanged.

``` r
peek_min(x)
#> [1] "task_b"
length(x)
#> [1] 3
```
