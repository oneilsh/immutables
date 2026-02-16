# Extract one element by position or unique name

Extract one element by position or unique name

## Usage

``` r
# S3 method for class 'flexseq'
x[[i, ...]]
```

## Arguments

- x:

  A \`flexseq\`.

- i:

  Positive scalar integer index, or scalar character element name.

- ...:

  Unused.

## Value

The extracted element (internal name metadata is removed).

## Examples

``` r
x <- as_flexseq(letters[1:5])
x[[3]]
#> [1] "c"

x2 <- as_flexseq(setNames(as.list(letters[1:3]), c("a1", "a2", "a3")))
x2[["a2"]]
#> [1] "b"
```
