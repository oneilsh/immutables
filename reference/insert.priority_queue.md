# Insert an element into a priority queue

Insert an element into a priority queue

## Usage

``` r
# S3 method for class 'priority_queue'
insert(x, element, priority, name = NULL, ...)
```

## Arguments

- x:

  A \`priority_queue\`.

- element:

  Element to insert.

- priority:

  Scalar non-missing orderable priority.

- name:

  Optional element name.

- ...:

  Unused.

## Value

Updated \`priority_queue\`.

## Examples

``` r
x <- priority_queue("a", "b", priorities = c(2, 1))
x
#> Unnamed priority_queue with 2 elements.
#> Minimum priority: 1, Maximum priority: 2
#> 
#> Elements (by priority):
#> 
#> (priority 1)
#> [1] "b"
#> 
#> (priority 2)
#> [1] "a"
#> 

x2 <- insert(x, "c", priority = 1)
x2
#> Unnamed priority_queue with 3 elements.
#> Minimum priority: 1, Maximum priority: 2
#> 
#> Elements (by priority):
#> 
#> (priority 1)
#> [1] "b"
#> 
#> (priority 1)
#> [1] "c"
#> 
#> (priority 2)
#> [1] "a"
#> 
peek_min(x2)
#> [1] "b"
```
