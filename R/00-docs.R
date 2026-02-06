#' Finger tree constructors and helpers
#'
#' These are documentation stubs for lambda.r-based functions. The real
#' implementations are defined via `%::%` / `%as%` and overwrite these.
#' @name fingertree-docs
#' @keywords internal
NULL

#' Generic node type
#' @param ... Elements in the node.
#' @export
Node <- function(...) { stop("Node is defined via lambda.r") }

#' Node with two elements
#' @param x First element.
#' @param y Second element.
#' @export
Node2 <- function(x, y) { stop("Node2 is defined via lambda.r") }

#' Node with three elements
#' @param x First element.
#' @param y Second element.
#' @param z Third element.
#' @export
Node3 <- function(x, y, z) { stop("Node3 is defined via lambda.r") }

#' Construct an empty FingerTree
#' @export
FingerTree <- function(...) { stop("FingerTree is defined via lambda.r") }

#' Empty FingerTree
#' @export
Empty <- function() { stop("Empty is defined via lambda.r") }

#' Single-element FingerTree
#' @param x Element to store.
#' @export
Single <- function(x) { stop("Single is defined via lambda.r") }

#' Digit (1 to 4 elements)
#' @param ... Elements in the digit.
#' @export
Digit <- function(...) { stop("Digit is defined via lambda.r") }

#' Deep FingerTree
#' @param prefix Prefix digit.
#' @param middle Middle tree.
#' @param suffix Suffix digit.
#' @export
Deep <- function(prefix, middle, suffix) { stop("Deep is defined via lambda.r") }

#' Reducer
#' @param f Reduction function.
#' @param i Identity element.
#' @export
Reducer <- function(f, i) { stop("Reducer is defined via lambda.r") }

#' Predicate
#' @param f Predicate function.
#' @export
Predicate <- function(f) { stop("Predicate is defined via lambda.r") }

#' Add element to the left
#' @param e Empty tree.
#' @param el Element to add.
#' @export
add_left <- function(e, el) { stop("add_left is defined via lambda.r") }

#' Add element to the right
#' @param e Empty tree.
#' @param el Element to add.
#' @export
add_right <- function(e, el) { stop("add_right is defined via lambda.r") }

#' Add multiple elements to the left
#' @param t FingerTree.
#' @param els Elements to add.
#' @export
add_all_left <- function(t, els) { stop("add_all_left is defined via lambda.r") }

#' Add multiple elements to the right
#' @param t FingerTree.
#' @param els Elements to add.
#' @export
add_all_right <- function(t, els) { stop("add_all_right is defined via lambda.r") }

#' Generalized concatenation helper
#' @param xs Left tree.
#' @param ts List of elements to insert between trees.
#' @param ys Right tree.
#' @export
app3 <- function(xs, ts, ys) { stop("app3 is defined via lambda.r") }

#' Concatenate two finger trees
#' @param xs Left tree.
#' @param ys Right tree.
#' @export
concat <- function(xs, ys) { stop("concat is defined via lambda.r") }

#' Build internal nodes from a list
#' @param l List of elements.
#' @export
nodes <- function(l) { stop("nodes is defined via lambda.r") }

#' Convert list to finger tree
#' @param l List or vector.
#' @export
as.FingerTree <- function(l, v = NULL) { stop("as.FingerTree is defined via lambda.r") }

#' Reduce from the left
#' @param t FingerTree.
#' @param r Reducer.
#' @export
reduce_left <- function(t, r) { stop("reduce_left is defined via lambda.r") }

#' Reduce from the right
#' @param t FingerTree.
#' @param r Reducer.
#' @export
reduce_right <- function(t, r) { stop("reduce_right is defined via lambda.r") }

#' Build graph data frames from a tree
#' @param t FingerTree.
#' @export
get_graph_df <- function(t) { stop("get_graph_df is defined via lambda.r") }

#' Plot a finger tree
#' @param t FingerTree.
#' @export
plot_tree <- function(t, ...) { stop("plot_tree is defined via lambda.r") }
