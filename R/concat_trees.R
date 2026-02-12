#' Concatenate two trees
#'
#' Same-name monoids are assumed equivalent; left-tree definitions win.
#' Missing monoids are added to each side before concatenation.
#'
#' @param x Left tree.
#' @param y Right tree.
#' @return Concatenated tree.
#' @examples
#' left <- tree_from(letters[1:3])
#' right <- tree_from(letters[4:6])
#' t <- concat_trees(left, right)
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_left(t, cat_m)
#'
#' # Concatenate trees carrying a custom monoid
#' sum_m <- MeasureMonoid(`+`, 0, as.numeric)
#' a <- tree_from(1:3, monoids = list(sum = sum_m))
#' b <- tree_from(4:5, monoids = list(sum = sum_m))
#' t2 <- suppressWarnings(concat_trees(a, b))
#' attr(t2, "measures")$sum
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
