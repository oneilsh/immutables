# Locate First Predicate Flip Without Reconstructing Context Trees

Read-only analogue of \[split_around_by_predicate()\]: finds the
distinguished element where the scan predicate flips, but does not
rebuild left/right trees.

## Usage

``` r
locate_by_predicate(
  t,
  predicate,
  monoid_name,
  accumulator = NULL,
  include_metadata = FALSE
)
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

- include_metadata:

  Logical; include left/hit/right measures and index.

## Value

If \`include_metadata = FALSE\`: \`list(found, elem)\`. If \`TRUE\`:
\`list(found, elem, metadata = list(left_measure, hit_measure,
right_measure, index))\`.

## Details

For \`priority_queue\` objects, \`metadata\$index\` (when requested) is
the internal structural position in the underlying sequence
representation. It is not related to priority rank and is not stable
across queue updates, so it should be treated as diagnostic metadata
only.
