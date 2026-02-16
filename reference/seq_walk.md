# Walk a Function Over Sequence Elements

Applies \`f\` to each element in left-to-right order for side effects.

## Usage

``` r
seq_walk(x, f, ...)
```

## Arguments

- x:

  A \`flexseq\`.

- f:

  Function applied to each element.

- ...:

  Additional arguments passed to \`f\`.

## Value

Invisibly returns \`x\`.

## Examples

``` r
x <- as_flexseq(1:4)
seq_walk(x, function(v) cat(v, "\\n"))
#> 1 \n2 \n3 \n4 \n
```
