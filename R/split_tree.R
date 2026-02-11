#' Split tree around first predicate flip
#'
#' @param t FingerTree.
#' @param predicate Function on measure values.
#' @param monoid_name Name of monoid from `attr(t, "monoids")`.
#' @param accumulator Optional starting measure (defaults to monoid identity).
#' @return A list with `left`, `elem`, and `right`.
#' @export
split_tree <- function(t, predicate, monoid_name, accumulator = NULL) {
  assert_structural_attrs(t)
  ctx <- resolve_named_monoid(t, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid

  if(t %isa% Empty) {
    stop("split_tree requires a non-empty tree.")
  }

  i <- if(is.null(accumulator)) mr$i else accumulator
  res <- split_tree_impl(predicate, i, t, ms, monoid_name)
  assert_structural_attrs(res$left)
  assert_structural_attrs(res$right)
  res
}
