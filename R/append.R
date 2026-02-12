#' Append an element
#'
#' @param t FingerTree.
#' @param x Element to append.
#' @return Updated tree.
#'   If `t` is name-indexed, appending unnamed elements is invalid.
#' @examples
#' t <- tree_from(letters[1:3])
#' t2 <- append(t, "d")
#' t2[[4]]
#'
#' # Compose with prepend and reduce
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_right(prepend(t2, "z"), cat_m)
#'
#' # Append to a named tree with an explicit element name
#' tn <- tree_from(setNames(as.list(1:2), c("a", "b")))
#' tn2 <- append(tn, stats::setNames(3, "c"))
#' tn2[["c"]]
#' @export
append <- function(t, x) {
  assert_structural_attrs(t)
  .ft_assert_name_state(t)
  ms <- resolve_tree_monoids(t, required = TRUE)
  x2 <- .ft_set_name(x, .ft_effective_name(x))
  t2 <- add_right(t, x2, ms)
  .ft_assert_name_state(t2)
  assert_structural_attrs(t2)
  t2
}
