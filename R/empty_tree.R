#' Create an Empty Structural Tree
#'
#' @param monoids Optional named list of `MeasureMonoid` objects.
#' @return An empty finger tree with structural `monoids` and `measures` attrs.
#' @examples
#' t <- empty_tree()
#' t
#'
#' count_m <- MeasureMonoid(`+`, 0, function(el) 1)
#' t2 <- empty_tree(monoids = list(count = count_m))
#' attr(t2, "measures")
#' @keywords internal
# Runtime: O(1).
empty_tree <- function(monoids = NULL) {
  ms <- if(is.null(monoids)) {
    list(.size = size_measure_monoid())
  } else {
    ensure_size_monoids(monoids)
  }
  measured_empty(ms)
}
