# Split a flexseq into Left and Right Parts by Predicate

Splits a sequence at the point where the predicate first becomes TRUE on
accumulated monoid measures.

## Usage

``` r
split_by_predicate(x, predicate, monoid_name)
```

## Arguments

- x:

  A \`flexseq\`.

- predicate:

  Predicate function on accumulated measure values.

- monoid_name:

  Character scalar naming the monoid to scan.

## Value

A list with \`left\` and \`right\` flexseq objects.
