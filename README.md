# immutables

[![R-CMD-check](https://github.com/oneilsh/immutables/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/oneilsh/immutables/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/oneilsh/immutables/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/oneilsh/immutables/actions/workflows/pkgdown.yaml)
[![docs](https://img.shields.io/badge/docs-pkgdown-blue)](https://oneilsh.github.io/immutables/)

The `immutables` R package implements sequence objects (`flexseq()`) supporting indexed and named access, appending and prepending, concatenation, splitting, fast push and pop from either end, item removal, and more.

Also implemented are priority queues (`priority_queue()`) 
supporting all of the above in addition to min and max
peeking and popping by priority value.

Backed by monoid-annotated 2-3 fingertrees as described by
[Hinze and Paterson](https://doi.org/10.1017/S0956796805005769), all structures are persistent (operations return effective modified copies), and most operations are constant time, amortized constant time, or $O(\log(n))$ (indexing $k$ elements is $O(k \log(n))$). 
Core functions are implemented in C++ (via Rcpp) for speed,
and parallel R implementations using `lambda.r` match their
counterparts in the paper.

The developer API supports the addition of custom structures 
via combinations of monoids and measures; see vignettes for
details.

- Hinze, R. and Paterson, R. (2006), *Finger trees: a simple general-purpose data structure*.
