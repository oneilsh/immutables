

# monoid fold helper; folds left over a tree or node structure
# Runtime: O(n) worst-case in relevant input/subtree size.
reduce_left_acc(t, r, acc) %::% . : . : . : .
reduce_left_acc(t, r, acc) %as% {
  if(is_structural_node(t) && t %isa% Empty) {
    return(acc)
  }
  if(!is_structural_node(t)) {
    return(r$f(acc, t))
  }
  if(is_structural_node(t) && t %isa% Single) {
    return(reduce_left_acc(.subset2(t, 1), r, acc))
  }
  if(is_structural_node(t) && t %isa% Deep) {
    acc <- reduce_left_acc(.subset2(t,"prefix"), r, acc)
    acc <- reduce_left_acc(.subset2(t,"middle"), r, acc)
    acc <- reduce_left_acc(.subset2(t,"suffix"), r, acc)
    return(acc)
  }
  for(el in t) {
    acc <- reduce_left_acc(el, r, acc)
  }
  return(acc)
}

# reduce_left_impl methods for different node types; uses identity once at top
# Runtime: O(n) worst-case in relevant input/subtree size.
reduce_left_impl(e, r) %::% Empty : MeasureMonoid : .
reduce_left_impl(e, r) %as% r$i



# Runtime: O(n) worst-case in relevant input/subtree size.
reduce_left_impl(s, r) %::% Single : MeasureMonoid : .
reduce_left_impl(s, r) %as% {
  reduce_left_acc(s, r, r$i)
}

# legacy Element wrapper (elements now can be any type)


# Runtime: O(n) worst-case in relevant input/subtree size.
reduce_left_impl(n, r) %::% Node : MeasureMonoid : .
reduce_left_impl(n, r) %as% {
  reduce_left_acc(n, r, r$i)
}

# Runtime: O(n) worst-case in relevant input/subtree size.
reduce_left_impl(d, r) %::% Digit : MeasureMonoid : .
reduce_left_impl(d, r) %as% {
  reduce_left_acc(d, r, r$i)
}


# reduce_left_impl for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
# Runtime: O(n) worst-case in relevant input/subtree size.
reduce_left_impl(t, r) %::% Deep : MeasureMonoid : .
reduce_left_impl(t, r) %as% {
  reduce_left_acc(t, r, r$i)
}

#' Reduce from the left
#'
#' @param t FingerTree.
#' @param monoid A `MeasureMonoid` object.
#' @return Reduced value.
#' @examples
#' t <- as_flexseq(1:5)
#' sum_m <- measure_monoid(`+`, 0, as.numeric)
#' reduce_left(t, sum_m)
#'
#' cat_m <- measure_monoid(paste0, "", as.character)
#' reduce_left(as_flexseq(letters[1:4]), cat_m)
#' @export
# Runtime: O(n) over number of elements.
reduce_left <- function(t, monoid) {
  if(!is_measure_monoid(monoid)) {
    stop("`monoid` must be a MeasureMonoid object.")
  }
  reduce_left_impl(t, monoid)
}
