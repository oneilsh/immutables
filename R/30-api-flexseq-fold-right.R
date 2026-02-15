

# monoid fold helper; folds right over a tree or node structure
# Runtime: O(n) worst-case in relevant input/subtree size.
fold_right_acc(t, r, acc) %::% . : . : . : .
fold_right_acc(t, r, acc) %as% {
  if(is_structural_node(t) && t %isa% Empty) {
    return(acc)
  }
  if(!is_structural_node(t)) {
    return(r$f(t, acc))
  }
  if(is_structural_node(t) && t %isa% Single) {
    return(fold_right_acc(.subset2(t, 1), r, acc))
  }
  if(is_structural_node(t) && t %isa% Deep) {
    acc <- fold_right_acc(.subset2(t,"suffix"), r, acc)
    acc <- fold_right_acc(.subset2(t,"middle"), r, acc)
    acc <- fold_right_acc(.subset2(t,"prefix"), r, acc)
    return(acc)
  }
  for(el in rev(t)) {
    acc <- fold_right_acc(el, r, acc)
  }
  return(acc)
}

# fold_right_impl methods for different node types; uses identity once at top
# Runtime: O(n) worst-case in relevant input/subtree size.
fold_right_impl(e, r) %::% Empty : MeasureMonoid : .
fold_right_impl(e, r) %as% r$i

# Runtime: O(n) worst-case in relevant input/subtree size.
fold_right_impl(s, r) %::% Single : MeasureMonoid : .
fold_right_impl(s, r) %as% {
  fold_right_acc(s, r, r$i)
}

# Runtime: O(n) worst-case in relevant input/subtree size.
fold_right_impl(n, r) %::% Node : MeasureMonoid : .
fold_right_impl(n, r) %as% {
  fold_right_acc(n, r, r$i)
}

# Runtime: O(n) worst-case in relevant input/subtree size.
fold_right_impl(d, r) %::% Digit : MeasureMonoid : .
fold_right_impl(d, r) %as% {
  fold_right_acc(d, r, r$i)
}

# Runtime: O(n) worst-case in relevant input/subtree size.
fold_right_impl(t, r) %::% Deep : MeasureMonoid : .
fold_right_impl(t, r) %as% {
  fold_right_acc(t, r, r$i)
}

#' Fold Right Over a Sequence
#'
#' @param x A `flexseq`.
#' @param monoid A `measure_monoid` specification.
#' @return Reduced value.
#' @examples
#' x <- as_flexseq(1:5)
#' x
#'
#' sum_m <- measure_monoid(`+`, 0, as.numeric)
#' fold_right(x, sum_m)
#'
#' x2 <- as_flexseq(letters[1:4])
#' cat_m <- measure_monoid(paste0, "", as.character)
#' fold_right(x2, cat_m)
#' @export
# Runtime: O(n) over number of elements.
fold_right <- function(x, monoid) {
  UseMethod("fold_right")
}

#' @method fold_right flexseq
#' @export
# Runtime: O(n) over number of elements.
fold_right.flexseq <- function(x, monoid) {
  if(!is_measure_monoid(monoid)) {
    stop("`monoid` must be a measure_monoid object.")
  }
  fold_right_impl(x, monoid)
}
