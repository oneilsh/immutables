#' Split tree into left and right parts
#'
#' @param t FingerTree.
#' @param predicate Function on measure values.
#' @param monoid_name Name of monoid from `attr(t, "monoids")`.
#' @return A list with `left` and `right`.
#' @export
split <- function(t, predicate, monoid_name) {
  assert_structural_attrs(t)
  ctx <- resolve_named_monoid(t, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid

  if(t %isa% Empty) {
    out <- list(left = measured_empty(ms), right = measured_empty(ms))
    assert_structural_attrs(out$left)
    assert_structural_attrs(out$right)
    return(out)
  }

  if(predicate(node_measure(t, monoid_name))) {
    s <- split_tree_impl(predicate, mr$i, t, ms, monoid_name)
    right <- prepend(s$right, s$elem)
    out <- list(left = s$left, right = right)
    assert_structural_attrs(out$left)
    assert_structural_attrs(out$right)
    return(out)
  }

  out <- list(left = t, right = measured_empty(ms))
  assert_structural_attrs(out$left)
  assert_structural_attrs(out$right)
  out
}
