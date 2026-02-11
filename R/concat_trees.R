#' Concatenate two trees
#'
#' Same-name monoids are assumed equivalent; left-tree definitions win.
#' Missing monoids are added to each side before concatenation.
#'
#' @param x Left tree.
#' @param y Right tree.
#' @return Concatenated tree.
#' @export
concat_trees <- function(x, y) {
  assert_structural_attrs(x)
  assert_structural_attrs(y)

  mx <- resolve_tree_monoids(x, required = TRUE)
  my <- resolve_tree_monoids(y, required = TRUE)

  shared <- intersect(names(mx), names(my))
  shared <- setdiff(shared, ".size")
  if(length(shared) > 0) {
    emit_concat_assumption_warning(shared)
  }

  left_only <- setdiff(names(mx), names(my))
  left_only <- setdiff(left_only, ".size")
  right_only <- setdiff(names(my), names(mx))
  right_only <- setdiff(right_only, ".size")

  x2 <- if(length(right_only) > 0) add_monoids(x, my[right_only], overwrite = FALSE) else x
  y2 <- if(length(left_only) > 0) add_monoids(y, mx[left_only], overwrite = FALSE) else y

  merged <- c(mx, my[setdiff(names(my), names(mx))])
  merged <- ensure_size_monoids(merged)

  t <- concat(x2, y2, merged)
  assert_structural_attrs(t)
  t
}
