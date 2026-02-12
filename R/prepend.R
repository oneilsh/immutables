#' Prepend an element
#'
#' @param t FingerTree.
#' @param x Element to prepend.
#' @return Updated tree.
#' @examples
#' t <- tree_from(letters[2:4])
#' t2 <- prepend(t, "a")
#' t2[[1]]
#'
#' # Compose with append and reduce
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_left(append(t2, "z"), cat_m)
#' @export
prepend <- function(t, x) {
  assert_structural_attrs(t)
  ms <- resolve_tree_monoids(t, required = TRUE)
  t2 <- add_left(t, x, ms)
  assert_structural_attrs(t2)
  t2
}
