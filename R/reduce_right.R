

# monoid fold helper; folds right over a tree or node structure
reduce_right_acc(t, r, acc) %::% . : . : . : .
reduce_right_acc(t, r, acc) %as% {
  if(is_structural_node(t) && t %isa% Empty) {
    return(acc)
  }
  if(!is_structural_node(t)) {
    return(r$f(t, acc))
  }
  if(is_structural_node(t) && t %isa% Single) {
    return(reduce_right_acc(.subset2(t, 1), r, acc))
  }
  if(is_structural_node(t) && t %isa% Deep) {
    acc <- reduce_right_acc(t$suffix, r, acc)
    acc <- reduce_right_acc(t$middle, r, acc)
    acc <- reduce_right_acc(t$prefix, r, acc)
    return(acc)
  }
  for(el in rev(t)) {
    acc <- reduce_right_acc(el, r, acc)
  }
  return(acc)
}

# reduce_right_impl methods for different node types; uses identity once at top
reduce_right_impl(e, r) %::% Empty : MeasureMonoid : .
reduce_right_impl(e, r) %as% r$i



reduce_right_impl(s, r) %::% Single : MeasureMonoid : .
reduce_right_impl(s, r) %as% {
  reduce_right_acc(s, r, r$i)
}

# legacy Element wrapper (elements now can be any type)
reduce_right_impl(n, r) %::% Node : MeasureMonoid : .
reduce_right_impl(n, r) %as% {
  reduce_right_acc(n, r, r$i)
}


# reduce_right_impl for digits, which can have 1 to 4 elements; again we just call the monoid function with the right grouping
reduce_right_impl(d, r) %::% Digit : MeasureMonoid : .
reduce_right_impl(d, r) %as% {
  reduce_right_acc(d, r, r$i)
}

# reduce_right_impl for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_right_impl(t, r) %::% Deep : MeasureMonoid : .
reduce_right_impl(t, r) %as% {
  reduce_right_acc(t, r, r$i)
}

#' Reduce from the right
#'
#' @param t FingerTree.
#' @param monoid A `MeasureMonoid` object.
#' @return Reduced value.
#' @examples
#' t <- tree_from(1:5)
#' sum_m <- MeasureMonoid(`+`, 0, as.numeric)
#' reduce_right(t, sum_m)
#'
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_right(tree_from(letters[1:4]), cat_m)
#' @export
reduce_right <- function(t, monoid) {
  if(!is_measure_monoid(monoid)) {
    stop("`monoid` must be a MeasureMonoid object.")
  }
  reduce_right_impl(t, monoid)
}
