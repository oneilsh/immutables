#' Create an empty finger tree
#'
#' @param monoids Optional named list of `MeasureMonoid` objects.
#' @return An empty finger tree with structural `monoids` and `measures` attrs.
#' @export
empty_tree <- function(monoids = NULL) {
  ms <- if(is.null(monoids)) {
    list(.size = size_measure_monoid())
  } else {
    ensure_size_monoids(monoids)
  }
  t <- measured_empty(ms)
  assert_structural_attrs(t)
  t
}
