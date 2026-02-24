# Print an ordered sequence summary

Print an ordered sequence summary

## Usage

``` r
# S3 method for class 'ordered_sequence'
print(x, max_elements = 4L, ...)
```

## Arguments

- x:

  An \`ordered_sequence\`.

- max_elements:

  Maximum number of elements shown in preview (\`head + tail\`).

- ...:

  Passed through to per-element \`print()\`.

## Value

Invisibly returns \`x\`.

## Examples

``` r
xs <- ordered_sequence(one = 1, two = 2, three = 3, keys = c(20, 30, 10))
print(xs, max_elements = 4)
#> Named ordered_sequence with 3 elements.
#> 
#> Elements (by key order):
#> 
#> $three (key 10)
#> [1] 3
#> 
#> $one (key 20)
#> [1] 1
#> 
#> $two (key 30)
#> [1] 2
#> 

ys <- ordered_sequence(1, 2, 3, keys = c(2, 1, 3))
print(ys, max_elements = 3)
#> Unnamed ordered_sequence with 3 elements.
#> 
#> Elements (by key order):
#> 
#> [[1]] (key 1)
#> [1] 2
#> 
#> [[2]] (key 2)
#> [1] 1
#> 
#> [[3]] (key 3)
#> [1] 3
#> 
```
