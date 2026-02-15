# Monoid-annotated 2-3 Finger Trees in R

Alpha tests on monoid-annotated fingertrees in R. Please note that this repository is very much an alpha work-in-progress; code is 
inefficient, functionality is missing. 

## Reference

Core structure and semantics are based on:

- Hinze, R. and Paterson, R. (2006), *Finger trees: a simple general-purpose data structure*.
  <https://www.cs.ox.ac.uk/ralf.hinze/publications/FingerTrees.pdf>

See `docs/architecture.md` for the reference-vs-fast backend contract and file-family layout.

## Runtime Options

Two options are useful when benchmarking/debugging internals:

- `options(immutables.use_cpp = TRUE/FALSE)`
  - Enables/disables C++ fast paths for append/prepend/indexing/split/locate/concat.
  - Default is `TRUE`.
- `options(immutables.validate_monoids = TRUE/FALSE)`
  - Enables extra monoid normalization checks inside hot-path monoid resolution.
  - Useful for debugging hand-mutated trees.
  - Default is `FALSE` (fast path).

`R/` - implementation files for the 2-3 finger tree and monoids.

`scripts/monoids_demo.R` - demo/exploration of monoid-annotated binary trees (not 2-3 finger trees).

`scripts/monoids_typed.R` - typed monoid/tree implementation using `lambda.r`.

`scripts/` - demo/experiment scripts for finger trees and plotting.
