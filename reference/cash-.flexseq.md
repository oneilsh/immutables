# Extract a named element with \`\$\`

Exact-name lookup on named trees, equivalent to \`x\[\[\\name\\\]\]\`.

## Usage

``` r
# S3 method for class 'flexseq'
x$name
```

## Arguments

- x:

  A \`flexseq\`.

- name:

  Element name.

## Value

The matched element.

## Examples

``` r
x <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
x$b
#> [1] 2
```
