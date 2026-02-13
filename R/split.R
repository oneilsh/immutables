#' Split tree into left and right parts
#'
#' @param t FingerTree.
#' @param predicate Function on measure values.
#' @param monoid_name Name of monoid from `attr(t, "monoids")`.
#' @return A list with `left` and `right`.
#' @examples
#' t <- tree_from(letters[1:6])
#' s <- split(t, function(v) v >= 4, ".size")
#' attr(s$left, "measures")$.size
#' attr(s$right, "measures")$.size
#'
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' fold_left(s$left, cat_m)
#' fold_left(s$right, cat_m)
#' @export
# Runtime: O(log n) near split point depth.
split <- function(t, predicate, monoid_name) {
  ctx <- resolve_named_monoid(t, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid

  if(t %isa% Empty) {
    return(list(left = .as_flexseq(measured_empty(ms)), right = .as_flexseq(measured_empty(ms))))
  }

  if(predicate(node_measure(t, monoid_name))) {
    s <- if(.ft_cpp_can_use(ms)) {
      .ft_cpp_split_tree(t, predicate, ms, monoid_name, mr$i)
    } else {
      split_tree_impl_fast(predicate, mr$i, t, ms, mr, monoid_name)
    }
    right <- prepend(s$right, s$elem)
    return(list(left = .as_flexseq(s$left), right = .as_flexseq(right)))
  }

  list(left = .as_flexseq(t), right = .as_flexseq(measured_empty(ms)))
}
