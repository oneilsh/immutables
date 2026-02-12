#' Construct a MeasureMonoid
#'
#' @param f Associative binary function over measure values.
#' @param i Identity measure value for `f`.
#' @param measure Function mapping a raw element to a measure value.
#' @return An object of class `MeasureMonoid`.
#' @examples
#' sum_m <- MeasureMonoid(`+`, 0, as.numeric)
#' t <- tree_from(1:5)
#' reduce_left(t, sum_m)
#'
#' # Measure from element metadata
#' t2 <- tree_from(letters[1:3], values = c(10, 20, 30))
#' value_sum <- MeasureMonoid(`+`, 0, function(el) as.numeric(attr(el, "value")))
#' reduce_right(t2, value_sum)
#' @export
MeasureMonoid <- function(f, i, measure) {
  res <- list(f = f, i = i, measure = measure)
  class(res) <- c("MeasureMonoid", class(res))
  res
}
