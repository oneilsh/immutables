# Split Around First Predicate Flip

Splits a sequence into left context, matched element, and right context
at the first point where \`predicate\` becomes \`TRUE\` on accumulated
monoid measures.

## Usage

``` r
split_around_by_predicate(t, predicate, monoid_name, accumulator = NULL)
```

## Arguments

- t:

  A \`flexseq\`.

- predicate:

  Function on accumulated measure values.

- monoid_name:

  Name of monoid from \`attr(t, "monoids")\`.

- accumulator:

  Optional starting measure (defaults to monoid identity).

## Value

A list with \`left\`, \`elem\`, and \`right\`.
