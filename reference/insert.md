# Insert an element into a priority queue

Insert an element into a priority queue

## Usage

``` r
insert(q, element, priority, name = NULL)
```

## Arguments

- q:

  A \`priority_queue\`.

- element:

  Element to insert.

- priority:

  Numeric scalar priority.

- name:

  Optional element name.

## Value

Updated \`priority_queue\`.

## Examples

``` r
x <- priority_queue("a", "b", priorities = c(2, 1))
x
#> <priority_queue> size=2 next_seq=3
#> min_priority=1 max_priority=2

x2 <- insert(x, "c", priority = 1)
x2
#> <priority_queue> size=3 next_seq=4
#> min_priority=1 max_priority=2
peek_min(x2)
#> [1] "b"
```
