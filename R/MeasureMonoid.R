#' Construct a MeasureMonoid
#'
#' @param f Associative binary function over measure values.
#' @param i Identity measure value for `f`.
#' @param measure Function mapping a raw element to a measure value.
#' @return An object of class `MeasureMonoid`.
#' @export
MeasureMonoid <- function(f, i, measure) {
  res <- list(f = f, i = i, measure = measure)
  class(res) <- c("MeasureMonoid", class(res))
  res
}
