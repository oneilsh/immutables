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

#' Build graph data frames from a tree
#' @param t FingerTree.
#' @export
get_graph_df <- function(t) { stop("get_graph_df is defined via lambda.r") }

#' Plot a finger tree
#' @param t FingerTree.
#' @export
plot_tree <- function(t, ...) { stop("plot_tree is defined via lambda.r") }
