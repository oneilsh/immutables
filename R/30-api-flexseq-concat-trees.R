#' Concatenate Two Structural Trees
#'
#' Same-name monoids are assumed equivalent; left-tree definitions win.
#' Missing monoids are added to each side before concatenation.
#'
#' @param x A `flexseq` (left side).
#' @param y A `flexseq` (right side).
#' @return Concatenated tree.
#' @examples
#' \dontrun{
#' left <- as_flexseq(letters[1:3])
#' right <- as_flexseq(letters[4:6])
#' t <- concat_trees(left, right)
#' as.list(t)
#' }
#' @keywords internal
# Runtime: O(nx + ny) when monoid harmonization requires add_monoids passes;
# otherwise concat spine work is near-logarithmic in boundary distance.
concat_trees <- function(x, y) {
  mx <- resolve_tree_monoids(x, required = TRUE)
  my <- resolve_tree_monoids(y, required = TRUE)

  shared <- intersect(names(mx), names(my))
  shared <- setdiff(shared, c(".size", ".named_count"))

  left_only <- setdiff(names(mx), names(my))
  left_only <- setdiff(left_only, c(".size", ".named_count"))
  right_only <- setdiff(names(my), names(mx))
  right_only <- setdiff(right_only, c(".size", ".named_count"))

  x2 <- if(length(right_only) > 0) add_monoids(x, my[right_only], overwrite = FALSE) else x
  y2 <- if(length(left_only) > 0) add_monoids(y, mx[left_only], overwrite = FALSE) else y

  merged <- c(mx, my[setdiff(names(my), names(mx))])
  merged <- ensure_size_monoids(merged)

  if(.ft_cpp_can_use(merged)) {
    return(.as_flexseq(.ft_cpp_concat(x2, y2, merged)))
  }

  .as_flexseq(concat(x2, y2, merged))
}
