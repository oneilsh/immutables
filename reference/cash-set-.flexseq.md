# Replace a named element with \`\$\<-\`

Exact-name replacement on named trees, equivalent to \`x\[\[\\name\\\]\]
\<- value\`.

## Usage

``` r
# S3 method for class 'flexseq'
x$name <- value
```

## Arguments

- x:

  A \`flexseq\`.

- name:

  Element name.

- value:

  Replacement element.

## Value

Updated tree.

## Examples

``` r
x <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
x$b <- 20
x$b
#> [1] 20
```
