# Subset a flexseq by position or element name

Subset a flexseq by position or element name

## Usage

``` r
# S3 method for class 'flexseq'
x[i, ...]
```

## Arguments

- x:

  A \`flexseq\`.

- i:

  Positive integer indices, character element names, or logical mask.

- ...:

  Unused.

## Value

A new \`flexseq\` containing selected elements in query order. For
character indexing, missing names are represented as \`NULL\` elements.

## Examples

``` r
x <- as_flexseq(letters[1:6])
x
#> FingerTree <size=6, named=no>
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

x2 <- x[c(2, 4, 6)]
x2
#> FingerTree <size=3, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "b"
#> 
#> [[2]]
#> [1] "d"
#> 
#> [[3]]
#> [1] "f"
#> 

# named lookups return NULL for missing names
x3 <- as_flexseq(setNames(as.list(letters[1:4]), c("w", "x", "y", "z")))
x4 <- x3[c("y", "missing", "w")]
x4
#> FingerTree <size=3, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "c"
#> 
#> [[2]]
#> NULL
#> 
#> [[3]]
#> [1] "a"
#> 

# logical indexing supports recycling
x[c(TRUE, FALSE)]
#> FingerTree <size=3, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "c"
#> 
#> [[3]]
#> [1] "e"
#> 
```
