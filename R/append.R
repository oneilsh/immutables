#' Append an element
#'
#' @param t FingerTree.
#' @param x Element to append.
#' @return Updated tree.
#' @export
append <- function(t, x) {
  assert_structural_attrs(t)
  ms <- resolve_tree_monoids(t, required = TRUE)
  t2 <- add_right(t, x, ms)
  assert_structural_attrs(t2)
  t2
}
