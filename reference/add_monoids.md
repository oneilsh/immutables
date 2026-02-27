# Add/merge monoids on an existing tree

Add/merge monoids on an existing tree

## Usage

``` r
add_monoids(t, monoids, overwrite = FALSE)
```

## Arguments

- t:

  FingerTree.

- monoids:

  Named list of \`measure_monoid\` objects to add.

- overwrite:

  Logical; whether overlapping names replace existing monoids.

## Value

A persistent copy with recomputed cached measures.

## See also

\[add_monoids.flexseq()\], \[add_monoids.priority_queue()\],
\[add_monoids.ordered_sequence()\], \[add_monoids.interval_index()\]
