resolve_tree_monoid <- function(t, monoid = NULL, required = FALSE) {
  r <- if(!is.null(monoid)) monoid else attr(t, "monoid")
  if(required && is.null(r)) {
    stop("No monoid provided and tree has no monoid attribute.")
  }
  if(!is.null(r) && !is_measure_monoid(r)) {
    stop("This package now requires a MeasureMonoid.")
  }
  r
}

resolve_concat_monoid <- function(x, y, monoid = NULL) {
  if(!is.null(monoid)) {
    return(resolve_tree_monoid(x, monoid, required = TRUE))
  }

  rx <- attr(x, "monoid")
  ry <- attr(y, "monoid")

  if(!is.null(rx) && !is_measure_monoid(rx)) {
    stop("Left tree has non-measured monoid; expected MeasureMonoid.")
  }
  if(!is.null(ry) && !is_measure_monoid(ry)) {
    stop("Right tree has non-measured monoid; expected MeasureMonoid.")
  }
  if(!is.null(rx) && !is.null(ry) && !identical(rx, ry)) {
    stop("Left/right trees have different monoid attributes. Pass an explicit monoid.")
  }

  r <- if(!is.null(rx)) rx else ry
  if(is.null(r)) {
    stop("No monoid provided and neither tree has a monoid attribute.")
  }
  r
}

#' Create an empty finger tree
#'
#' @param monoid Measure monoid stored on the tree.
#' @return An empty finger tree.
#' @examples
#' r <- MeasureMonoid(function(a, b) a + b, 0, function(el) 0)
#' t <- empty_tree(monoid = r)
#' @export
empty_tree <- function(monoid = NULL) {
  r <- resolve_tree_monoid(Empty(), monoid, required = TRUE)
  t <- measured_empty(r)
  attr(t, "monoid") <- r
  t
}

#' Build a tree from a vector or list
#'
#' @param x Elements to insert.
#' @param values Optional parallel values (same length as x). Stored as the
#'   `value` attribute on each element when provided.
#' @param monoid Measure monoid stored on the tree.
#' @return A finger tree containing the elements.
#' @examples
#' r <- MeasureMonoid(function(a, b) a + b, 0, function(el) 0)
#' t <- tree_from(1:3, monoid = r)
#' @export
tree_from <- function(x, values = NULL, monoid = NULL) {
  r <- resolve_tree_monoid(Empty(), monoid, required = TRUE)
  t <- empty_tree(r)
  x_list <- as.list(x)
  if(is.null(values)) {
    for(el in x_list) {
      t <- append(t, el, r)
    }
  } else {
    v_list <- as.list(values)
    if(length(x_list) != length(v_list)) {
      stop("length of entries and values lists given to tree_from not equal.")
    }
    for(i in seq_along(x_list)) {
      el <- x_list[[i]]
      attr(el, "value") <- v_list[[i]]
      t <- append(t, el, r)
    }
  }
  attr(t, "monoid") <- r
  t
}

#' Prepend an element
#'
#' @param t FingerTree.
#' @param x Element to prepend.
#' @param monoid Optional measure monoid; falls back to tree attribute.
#' @return Updated tree.
#' @export
prepend <- function(t, x, monoid = NULL) {
  r <- resolve_tree_monoid(t, monoid, required = TRUE)
  t2 <- add_left(t, x, r)
  if(!is.null(r)) { attr(t2, "monoid") <- r }
  t2
}

#' Append an element
#'
#' @param t FingerTree.
#' @param x Element to append.
#' @param monoid Optional measure monoid; falls back to tree attribute.
#' @return Updated tree.
#' @export
append <- function(t, x, monoid = NULL) {
  r <- resolve_tree_monoid(t, monoid, required = TRUE)
  t2 <- add_right(t, x, r)
  if(!is.null(r)) { attr(t2, "monoid") <- r }
  t2
}

#' Concatenate two trees
#'
#' @param x Left tree.
#' @param y Right tree.
#' @param monoid Optional measure monoid. If omitted, uses left/right tree
#'   attributes; errors if neither tree has one or both differ.
#' @return Concatenated tree.
#' @export
concat_trees <- function(x, y, monoid = NULL) {
  r <- resolve_concat_monoid(x, y, monoid)
  t <- concat(x, y, r)
  if(!is.null(r)) { attr(t, "monoid") <- r }
  t
}

#' Reduce from the left
#'
#' @param t FingerTree.
#' @param monoid Optional measure monoid; falls back to tree attribute.
#' @return Reduced value.
#' @export
reduce_left <- function(t, monoid = NULL) {
  r <- resolve_tree_monoid(t, monoid, required = TRUE)
  reduce_left_impl(t, r)
}

#' Reduce from the right
#'
#' @param t FingerTree.
#' @param monoid Optional measure monoid; falls back to tree attribute.
#' @return Reduced value.
#' @export
reduce_right <- function(t, monoid = NULL) {
  r <- resolve_tree_monoid(t, monoid, required = TRUE)
  reduce_right_impl(t, r)
}

#' Split tree around first predicate flip
#'
#' @param t FingerTree.
#' @param predicate Function on measure values.
#' @param monoid Optional measure monoid; falls back to tree attribute.
#' @param accumulator Optional starting measure (defaults to monoid identity).
#' @return A list with `left`, `elem`, and `right`.
#' @export
split_tree <- function(t, predicate, monoid = NULL, accumulator = NULL) {
  r <- resolve_tree_monoid(t, monoid, required = TRUE)
  if(t %isa% Empty) {
    stop("split_tree requires a non-empty tree.")
  }
  i <- if(is.null(accumulator)) r$i else accumulator
  res <- split_tree_impl(predicate, i, t, r)
  attr(res$left, "monoid") <- r
  attr(res$right, "monoid") <- r
  res
}

#' Split tree into left and right parts
#'
#' @param t FingerTree.
#' @param predicate Function on measure values.
#' @param monoid Optional measure monoid; falls back to tree attribute.
#' @return A list with `left` and `right`.
#' @export
split <- function(t, predicate, monoid = NULL) {
  r <- resolve_tree_monoid(t, monoid, required = TRUE)

  if(t %isa% Empty) {
    out <- list(left = measured_empty(r), right = measured_empty(r))
    attr(out$left, "monoid") <- r
    attr(out$right, "monoid") <- r
    return(out)
  }

  if(predicate(measure_child(t, r))) {
    s <- split_tree_impl(predicate, r$i, t, r)
    right <- prepend(s$right, s$elem, r)
    attr(s$left, "monoid") <- r
    attr(right, "monoid") <- r
    return(list(left = s$left, right = right))
  }

  out <- list(left = t, right = measured_empty(r))
  attr(out$left, "monoid") <- r
  attr(out$right, "monoid") <- r
  out
}
