#' immutables
#' 
#' Sequence objects (`flexseq()`) supporting indexed and named access,
#' appending and prepending, concatenation, splitting, fast push
#' and pop from either end, item removal, and more. 
#' 
#' Also implemented are priority queues (`priority_queue()`) 
#' supporting all of the above in addition to min and max
#' peeking and popping by priority value.
#' 
#' Backed by monoid-annotated 2-3 fingertrees, all structures are
#' persistent (operations return effective modified copies), and
#' most operations are constant time, amortized constant time, or
#' \eqn{O(\log n)} (indexing k elements is \eqn{O(k \log n)}). The
#' developer API supports the addition of custom structures 
#' via combinations of monoids and measures; see vignettes for
#' details.
#'
#' @keywords internal
#' @import lambda.r
#' @importFrom Rcpp evalCpp
#' @useDynLib immutables, .registration = TRUE
"_PACKAGE"
