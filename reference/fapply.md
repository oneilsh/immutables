# Fapply with S3 dispatch

\`fapply()\` is an S3 generic for applying functions over immutable
structures with type-specific dispatch.

## Usage

``` r
fapply(X, FUN, ...)
```

## Arguments

- X:

  Object to apply over.

- FUN:

  Function to apply.

- ...:

  Method-specific arguments.

## Value

Method-dependent result.

## See also

\[fapply.flexseq()\], \[fapply.priority_queue()\],
\[fapply.ordered_sequence()\], \[fapply.interval_index()\]
