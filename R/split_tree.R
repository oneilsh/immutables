#' Split tree around first predicate flip
#'
#' @param t FingerTree.
#' @param predicate Function on measure values.
#' @param monoid_name Name of monoid from `attr(t, "monoids")`.
#' @param accumulator Optional starting measure (defaults to monoid identity).
#' @return A list with `left`, `elem`, and `right`.
#' @examples
#' t <- tree_from(letters[1:6])
#' s <- split_tree(t, function(v) v >= 4, ".size")
#' s$elem
#'
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_left(s$left, cat_m)
#' reduce_left(s$right, cat_m)
#' @export
# Runtime: O(log n) near split point depth.
split_tree <- function(t, predicate, monoid_name, accumulator = NULL) {
  ctx <- resolve_named_monoid(t, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid

  if(t %isa% Empty) {
    stop("split_tree requires a non-empty tree.")
  }

  i <- if(is.null(accumulator)) mr$i else accumulator
  split_tree_impl(predicate, i, t, ms, monoid_name)
}
