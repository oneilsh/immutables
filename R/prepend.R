#' Prepend an element
#'
#' @param t FingerTree.
#' @param x Element to prepend.
#' @return Updated tree.
#'   If `t` is name-indexed, prepending unnamed elements is invalid.
#' @examples
#' t <- tree_from(letters[2:4])
#' t2 <- prepend(t, "a")
#' t2[[1]]
#'
#' # Compose with append and reduce
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_left(append(t2, "z"), cat_m)
#'
#' # Prepend to a named tree with an explicit element name
#' tn <- tree_from(setNames(as.list(2:3), c("b", "c")))
#' tn2 <- prepend(tn, stats::setNames(1, "a"))
#' tn2[["a"]]
#' @export
prepend <- function(t, x) {
  assert_structural_attrs(t)
  .ft_assert_name_state(t)
  ms <- resolve_tree_monoids(t, required = TRUE)
  x2 <- .ft_set_name(x, .ft_effective_name(x))
  t2 <- add_left(t, x2, ms)
  .ft_assert_name_state(t2)
  assert_structural_attrs(t2)
  t2
}
