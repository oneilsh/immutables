#' Finger tree public API
#'
#' Documentation stubs for exported functions. Implementations live in the
#' lambda.r definitions or in `R/api.R`.
#' @name fingertree-docs
#' @keywords internal
NULL

#' Reducer
#' @param f Reduction function.
#' @param i Identity element.
#' @export
Reducer <- function(f, i) { stop("Reducer is defined via lambda.r") }

#' Predicate
#' @param f Predicate function.
#' @export
Predicate <- function(f) { stop("Predicate is defined via lambda.r") }

#' Create an empty tree
#' @param reducer Optional reducer to store on the tree.
#' @export
empty_tree <- function(reducer = NULL) { stop("empty_tree is defined in api.R") }

#' Build a tree from a vector or list
#' @param x Elements to insert.
#' @param values Optional values for each element.
#' @param reducer Optional reducer to store on the tree.
#' @export
tree_from <- function(x, values = NULL, reducer = NULL) { stop("tree_from is defined in api.R") }

#' Prepend an element
#' @param t FingerTree.
#' @param x Element to prepend.
#' @param reducer Optional reducer to store on the tree.
#' @export
prepend <- function(t, x, reducer = NULL) { stop("prepend is defined in api.R") }

#' Append an element
#' @param t FingerTree.
#' @param x Element to append.
#' @param reducer Optional reducer to store on the tree.
#' @export
append <- function(t, x, reducer = NULL) { stop("append is defined in api.R") }

#' Concatenate two trees
#' @param x Left tree.
#' @param y Right tree.
#' @param reducer Optional reducer to store on the tree.
#' @export
concat_trees <- function(x, y, reducer = NULL) { stop("concat_trees is defined in api.R") }

#' Reduce from the left
#' @param t FingerTree.
#' @param reducer Optional reducer; falls back to tree attribute.
#' @export
reduce_left <- function(t, reducer = NULL) { stop("reduce_left is defined in api.R") }

#' Reduce from the right
#' @param t FingerTree.
#' @param reducer Optional reducer; falls back to tree attribute.
#' @export
reduce_right <- function(t, reducer = NULL) { stop("reduce_right is defined in api.R") }

#' Build graph data frames from a tree
#' @param t FingerTree.
#' @export
get_graph_df <- function(t) { stop("get_graph_df is defined via lambda.r") }

#' Plot a finger tree
#' @param t FingerTree.
#' @export
plot_tree <- function(t, ...) { stop("plot_tree is defined via lambda.r") }
