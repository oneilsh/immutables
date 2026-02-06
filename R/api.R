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

empty_tree <- function(reducer = NULL) {
  t <- Empty()
  if(!is.null(reducer)) {
    attr(t, "reducer") <- reducer
  }
  t
}

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

prepend <- function(t, x, reducer = NULL) {
  t2 <- add_left(t, x)
  r <- resolve_tree_reducer(t, reducer, required = FALSE)
  if(!is.null(r)) {
    attr(t2, "reducer") <- r
  }
  t2
}

append <- function(t, x, reducer = NULL) {
  t2 <- add_right(t, x)
  r <- resolve_tree_reducer(t, reducer, required = FALSE)
  if(!is.null(r)) {
    attr(t2, "reducer") <- r
  }
  t2
}

concat_trees <- function(x, y, reducer = NULL) {
  t <- concat(x, y)
  r <- resolve_tree_reducer(x, reducer, required = FALSE)
  if(!is.null(r)) {
    attr(t, "reducer") <- r
  }
  t
}

reduce_left <- function(t, reducer = NULL) {
  r <- resolve_tree_reducer(t, reducer, required = TRUE)
  reduce_left_impl(t, r)
}

reduce_right <- function(t, reducer = NULL) {
  r <- resolve_tree_reducer(t, reducer, required = TRUE)
  reduce_right_impl(t, r)
}
