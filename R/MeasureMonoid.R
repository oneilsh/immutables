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
#' # Measure from element metadata
#' t2 <- as_flexseq(letters[1:3], values = c(10, 20, 30))
#' value_sum <- measure_monoid(`+`, 0, function(el) as.numeric(attr(el, "value")))
#' t2m <- add_monoids(t2, list(value_sum = value_sum))
#' attr(t2m, "measures")$value_sum
#' @export
# Runtime: O(1).
measure_monoid <- function(f, i, measure) {
  res <- list(f = f, i = i, measure = measure)
  class(res) <- c("measure_monoid", "MeasureMonoid", class(res))
  res
}

# Runtime: O(1).
MeasureMonoid <- function(f, i, measure) {
  measure_monoid(f, i, measure)
}
