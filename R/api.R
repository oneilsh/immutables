resolve_tree_reducer <- function(t, reducer = NULL, required = FALSE) {
  if(!is.null(reducer)) {
    return(reducer)
  }
  r <- attr(t, "reducer")
  if(required && is.null(r)) {
    stop("No reducer provided and tree has no reducer attribute.")
  }
  r
}

set_tree_reducer <- function(t, reducer) {
  attr(t, "reducer") <- reducer
  t
}

#' Create an empty finger tree
#'
#' @param reducer Optional reducer stored on the tree for later use.
#' @return An empty finger tree.
#' @examples
#' r <- Reducer(function(a, b) a + b, 0)
#' t <- empty_tree(reducer = r)
#' @export
empty_tree <- function(reducer = NULL) {
  t <- Empty()
  if(!is.null(reducer)) {
    attr(t, "reducer") <- reducer
  }
  t
}

#' Build a tree from a vector or list
#'
#' @param x Elements to insert.
#' @param values Optional parallel values (same length as x). Stored as the
#'   `value` attribute on each element when provided.
#' @param reducer Optional reducer stored on the tree for later use.
#' @return A finger tree containing the elements.
#' @examples
#' t <- tree_from(1:3)
#' @export
tree_from <- function(x, values = NULL, reducer = NULL) {
  t <- if(is.null(values)) {
    as.FingerTree(x)
  } else {
    as.FingerTree(x, values)
  }
  if(!is.null(reducer)) {
    attr(t, "reducer") <- reducer
  }
  t
}

#' Prepend an element
#'
#' @param t FingerTree.
#' @param x Element to prepend.
#' @param reducer Optional reducer to store on the tree.
#' @return Updated tree.
#' @export
prepend <- function(t, x, reducer = NULL) {
  t2 <- add_left(t, x)
  r <- resolve_tree_reducer(t, reducer, required = FALSE)
  if(!is.null(r)) {
    attr(t2, "reducer") <- r
  }
  t2
}

#' Append an element
#'
#' @param t FingerTree.
#' @param x Element to append.
#' @param reducer Optional reducer to store on the tree.
#' @return Updated tree.
#' @export
append <- function(t, x, reducer = NULL) {
  t2 <- add_right(t, x)
  r <- resolve_tree_reducer(t, reducer, required = FALSE)
  if(!is.null(r)) {
    attr(t2, "reducer") <- r
  }
  t2
}

#' Concatenate two trees
#'
#' @param x Left tree.
#' @param y Right tree.
#' @param reducer Optional reducer to store on the result.
#' @return Concatenated tree.
#' @export
concat_trees <- function(x, y, reducer = NULL) {
  t <- concat(x, y)
  r <- resolve_tree_reducer(x, reducer, required = FALSE)
  if(!is.null(r)) {
    attr(t, "reducer") <- r
  }
  t
}

#' Reduce from the left
#'
#' @param t FingerTree.
#' @param reducer Optional reducer; falls back to tree attribute.
#' @return Reduced value.
#' @export
reduce_left <- function(t, reducer = NULL) {
  r <- resolve_tree_reducer(t, reducer, required = TRUE)
  reduce_left_impl(t, r)
}

#' Reduce from the right
#'
#' @param t FingerTree.
#' @param reducer Optional reducer; falls back to tree attribute.
#' @return Reduced value.
#' @export
reduce_right <- function(t, reducer = NULL) {
  r <- resolve_tree_reducer(t, reducer, required = TRUE)
  reduce_right_impl(t, r)
}
