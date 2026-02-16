# Print a compact summary of a finger tree

Print a compact summary of a finger tree

## Usage

``` r
# S3 method for class 'FingerTree'
print(x, max_elements = 6L, show_internal_monoids = FALSE, ...)

# S3 method for class 'Deep'
print(x, ...)

# S3 method for class 'Single'
print(x, ...)

# S3 method for class 'Empty'
print(x, ...)
```

## Arguments

- x:

  FingerTree.

- max_elements:

  Maximum number of elements to show in list-style preview. Default
  \`6\`.

- show_internal_monoids:

  Logical; show internal monoids (\`.size\`, \`.named_count\`). Default
  \`FALSE\`.

- ...:

  Passed through to \`print()\` for preview elements.

## Value

\`x\`, invisibly.

## Examples

``` r
x <- as_flexseq(letters[1:10])
x
#> FingerTree <size=10, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> [[3]]
#> [1] "c"
#> 
#> [[4]]
#> [1] "d"
#> 
#> [[5]]
#> [1] "e"
#> 
#> [[6]]
#> [1] "f"
#> 
#> ... and 4 more elements not shown

x2 <- as_flexseq(setNames(as.list(1:5), paste0("k", 1:5)))
x2
#> FingerTree <size=5, named=yes>
#>   monoids: none
#> 
#> $k1
#> [1] 1
#> 
#> $k2
#> [1] 2
#> 
#> $k3
#> [1] 3
#> 
#> $k4
#> [1] 4
#> 
#> $k5
#> [1] 5
#> 
```
