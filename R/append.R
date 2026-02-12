#' Append an element
#'
#' @param t FingerTree.
#' @param x Element to append.
#' @return Updated tree.
#' @examples
#' t <- tree_from(letters[1:3])
#' t2 <- append(t, "d")
#' t2[[4]]
#'
#' # Compose with prepend and reduce
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_right(prepend(t2, "z"), cat_m)
#' @export
append <- function(t, x) {
  assert_structural_attrs(t)
  ms <- resolve_tree_monoids(t, required = TRUE)
  t2 <- add_right(t, x, ms)
  assert_structural_attrs(t2)
  t2
}
