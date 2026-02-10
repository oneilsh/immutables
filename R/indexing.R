# flatten a fingertree into its element sequence (left-to-right)
.ft_to_list <- function(x) {
  if(!is_structural_node(x)) {
    return(list(x))
  }
  if(x %isa% Empty) {
    return(list())
  }
  if(x %isa% Single) {
    return(.ft_to_list(.subset2(x, 1)))
  }
  if(x %isa% Deep) {
    return(c(.ft_to_list(x$prefix), .ft_to_list(x$middle), .ft_to_list(x$suffix)))
  }
  out <- list()
  for(el in x) {
    out <- c(out, .ft_to_list(el))
  }
  out
}

.ft_assert_int_indices <- function(idx, n) {
  if(is.null(idx)) {
    stop("Index is required.")
  }
  if(!is.numeric(idx) || any(is.na(idx)) || any(idx != as.integer(idx))) {
    stop("Only non-missing integer indices are supported.")
  }
  idx <- as.integer(idx)
  if(length(idx) == 0L) {
    return(idx)
  }
  if(any(idx <= 0L)) {
    stop("Only positive integer indices are supported.")
  }
  if(any(idx > n)) {
    stop("Index out of bounds.")
  }
  idx
}

#' Subset a finger tree by positive integer indices
#'
#' @param x FingerTree.
#' @param i Positive integer index vector.
#' @param ... Unused.
#' @return A new FingerTree containing selected elements in index order.
#' @export
`[.FingerTree` <- function(x, i, ...) {
  if(missing(i)) {
    return(x)
  }
  r <- resolve_tree_monoid(x, required = FALSE)
  if(is.null(r)) {
    y <- unclass(x)
    out <- y[i]
    class(out) <- class(x)
    return(out)
  }
  xs <- .ft_to_list(x)
  idx <- .ft_assert_int_indices(i, length(xs))
  out <- if(length(idx) == 0L) list() else xs[idx]
  tree_from(out, monoid = r)
}

#' Extract a single element from a finger tree by 1-based index
#'
#' @param x FingerTree.
#' @param i Positive scalar integer index.
#' @param ... Unused.
#' @return The extracted element.
#' @export
`[[.FingerTree` <- function(x, i, ...) {
  if(is.character(i) && length(i) == 1L && !is.na(i)) {
    return(.subset2(x, i))
  }
  r <- resolve_tree_monoid(x, required = FALSE)
  if(is.null(r)) {
    return(.subset2(unclass(x), i))
  }
  xs <- .ft_to_list(x)
  idx <- .ft_assert_int_indices(i, length(xs))
  if(length(idx) != 1L) {
    stop("[[ expects exactly one index.")
  }
  xs[[idx]]
}

#' Replace selected elements by positive integer indices
#'
#' @param x FingerTree.
#' @param i Positive integer index vector.
#' @param value Replacement values; must have exactly same length as `i`.
#' @return A new FingerTree with selected elements replaced.
#' @export
`[<-.FingerTree` <- function(x, i, value) {
  r <- resolve_tree_monoid(x, required = FALSE)
  if(is.null(r)) {
    attrs <- attributes(x)
    y <- unclass(x)
    y[i] <- value
    attributes(y) <- attrs
    return(y)
  }
  xs <- .ft_to_list(x)
  idx <- .ft_assert_int_indices(i, length(xs))
  vals <- as.list(value)
  if(length(vals) != length(idx)) {
    stop("Replacement length must match index length exactly.")
  }
  for(k in seq_along(idx)) {
    xs[[idx[[k]]]] <- vals[[k]]
  }
  tree_from(xs, monoid = r)
}

#' Replace a single element by 1-based index
#'
#' @param x FingerTree.
#' @param i Positive scalar integer index.
#' @param value Replacement element.
#' @return A new FingerTree with one element replaced.
#' @export
`[[<-.FingerTree` <- function(x, i, value) {
  if(is.character(i) && length(i) == 1L && !is.na(i)) {
    attrs <- attributes(x)
    y <- unclass(x)
    y[[i]] <- value
    attributes(y) <- attrs
    return(y)
  }
  r <- resolve_tree_monoid(x, required = FALSE)
  if(is.null(r)) {
    attrs <- attributes(x)
    y <- unclass(x)
    y[[i]] <- value
    attributes(y) <- attrs
    return(y)
  }
  xs <- .ft_to_list(x)
  idx <- .ft_assert_int_indices(i, length(xs))
  if(length(idx) != 1L) {
    stop("[[<- expects exactly one index.")
  }
  xs[[idx]] <- value
  tree_from(xs, monoid = r)
}
