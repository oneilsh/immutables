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
    if(is_measure_reducer(reducer)) {
      t <- measured_empty(reducer)
      attr(t, "reducer") <- reducer
    }
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
  if(is_measure_reducer(reducer)) {
    t <- empty_tree(reducer)
    x_list <- as.list(x)
    if(is.null(values)) {
      for(el in x_list) {
        t <- append(t, el, reducer)
      }
    } else {
      v_list <- as.list(values)
      if(length(x_list) != length(v_list)) {
        stop("length of entries and values lists given to tree_from not equal.")
      }
      for(i in seq_along(x_list)) {
        el <- x_list[[i]]
        attr(el, "value") <- v_list[[i]]
        t <- append(t, el, reducer)
      }
    }
  } else {
    t <- if(is.null(values)) {
      as.FingerTree(x)
    } else {
      as.FingerTree(x, values)
    }
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
  r <- resolve_tree_reducer(t, reducer, required = FALSE)
  t2 <- if(is_measure_reducer(r)) add_left(t, x, r) else add_left(t, x)
  if(!is.null(r)) { attr(t2, "reducer") <- r }
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
  r <- resolve_tree_reducer(t, reducer, required = FALSE)
  t2 <- if(is_measure_reducer(r)) add_right(t, x, r) else add_right(t, x)
  if(!is.null(r)) { attr(t2, "reducer") <- r }
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
  r <- resolve_tree_reducer(x, reducer, required = FALSE)
  t <- if(is_measure_reducer(r)) concat(x, y, r) else concat(x, y)
  if(!is.null(r)) { attr(t, "reducer") <- r }
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
