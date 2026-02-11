#' Prepend an element
#'
#' @param t FingerTree.
#' @param x Element to prepend.
#' @return Updated tree.
#' @export
prepend <- function(t, x) {
  assert_structural_attrs(t)
  ms <- resolve_tree_monoids(t, required = TRUE)
  t2 <- add_left(t, x, ms)
  assert_structural_attrs(t2)
  t2
}
