#' Construct a Measure Monoid Specification
#'
#' @param f Associative binary function over measure values.
#' @param i Identity measure value for `f`.
#' @param measure Function mapping a raw element to a measure value.
#' @return An object of class `measure_monoid`.
#' @examples
#' sum_m <- measure_monoid(`+`, 0, as.numeric)
#' t <- as_flexseq(1:5)
#' fold_left(t, sum_m)
#'
#' # Add a custom monoid after construction
#' nchar_sum <- measure_monoid(`+`, 0, function(el) nchar(as.character(el)))
#' t2 <- add_monoids(as_flexseq(letters[1:3]), list(nchar_sum = nchar_sum))
#' attr(t2, "measures")$nchar_sum
#' @export
# Runtime: O(1).
measure_monoid <- function(f, i, measure) {
  res <- list(f = f, i = i, measure = measure)
  class(res) <- c("measure_monoid", "MeasureMonoid", class(res))
  res
}
