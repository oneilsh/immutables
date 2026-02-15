#' Split tree around first predicate flip
#'
#' @param t A `flexseq`.
#' @param predicate Function on measure values.
#' @param monoid_name Name of monoid from `attr(t, "monoids")`.
#' @param accumulator Optional starting measure (defaults to monoid identity).
#' @return A list with `left`, `elem`, and `right`.
#' @examples
#' x <- as_flexseq(letters[1:6])
#' x
#'
#' s <- split_tree(x, function(v) v >= 4, ".size")
#' s$elem
#' s$left
#' s$right
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
  out <- if(.ft_cpp_can_use(ms)) {
    .ft_cpp_split_tree(t, predicate, ms, monoid_name, i)
  } else {
    split_tree_impl_fast(predicate, i, t, ms, mr, monoid_name)
  }
  out$left <- .as_flexseq(out$left)
  out$right <- .as_flexseq(out$right)
  out
}
