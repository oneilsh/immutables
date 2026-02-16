# Split by Scalar Index or Name

Splits by element position (\`.size\` measure) after resolving \`at\` to
a single index. \`at\` can be a positive scalar integer index or a
scalar character name.

## Usage

``` r
split_at(x, at, pull_index = FALSE)
```

## Arguments

- x:

  A \`flexseq\`.

- at:

  Positive scalar integer index or scalar character name.

- pull_index:

  Logical switch between two-way and three-way split shape. If \`TRUE\`,
  uses \[split_by_predicate()\] and returns \`list(left, right)\`. If
  \`FALSE\`, uses \[split_around_by_predicate()\] and returns
  \`list(left, elem, right)\`.

## Value

A split list, shape controlled by \`pull_index\`.

## Examples

``` r
x <- as_flexseq(setNames(as.list(letters[1:6]), LETTERS[1:6]))
split_at(x, 3)
#> $left
#> FingerTree <size=2, named=yes>
#>   monoids: none
#> 
#> $A
#> [1] "a"
#> 
#> $B
#> [1] "b"
#> 
#> 
#> $elem
#> [1] "c"
#> attr(,"ft_name")
#> [1] "C"
#> 
#> $right
#> FingerTree <size=3, named=yes>
#>   monoids: none
#> 
#> $D
#> [1] "d"
#> 
#> $E
#> [1] "e"
#> 
#> $F
#> [1] "f"
#> 
#> 
split_at(x, "C", pull_index = TRUE)
#> $left
#> FingerTree <size=2, named=yes>
#>   monoids: none
#> 
#> $A
#> [1] "a"
#> 
#> $B
#> [1] "b"
#> 
#> 
#> $right
#> FingerTree <size=4, named=yes>
#>   monoids: none
#> 
#> $C
#> [1] "c"
#> 
#> $D
#> [1] "d"
#> 
#> $E
#> [1] "e"
#> 
#> $F
#> [1] "f"
#> 
#> 
```
