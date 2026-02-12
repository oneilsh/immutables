# flatten a fingertree into its element sequence (left-to-right)
.ft_to_list(x) %::% . : list
.ft_to_list(x) %as% {
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

.ft_assert_int_indices(idx, n) %::% . : numeric : integer
.ft_assert_int_indices(idx, n) %as% {
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
#' @examples
#' t <- tree_from(letters[1:6])
#' s <- t[c(2, 4, 6)]
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_left(s, cat_m)
#'
#' # Empty index returns empty tree
#' attr(t[integer(0)], "measures")$.size
#' @export
`[.FingerTree` <- function(x, i, ...) {
  if(missing(i)) {
    return(x)
  }
  ms <- resolve_tree_monoids(x, required = TRUE)
  xs <- .ft_to_list(x)
  idx <- .ft_assert_int_indices(i, length(xs))
  out <- if(length(idx) == 0L) list() else xs[idx]
  tree_from(out, monoids = ms)
}

#' Extract a single element from a finger tree by 1-based index
#'
#' @param x FingerTree.
#' @param i Positive scalar integer index.
#' @param ... Unused.
#' @return The extracted element.
#' @examples
#' t <- tree_from(letters[1:5])
#' t[[3]]
#'
#' # Named extraction for structural fields (internal/debug usage)
#' d <- tree_from(letters[1:3])
#' d[["prefix"]]
#' @export
`[[.FingerTree` <- function(x, i, ...) {
  if(is.character(i) && length(i) == 1L && !is.na(i)) {
    return(.subset2(x, i))
  }
  resolve_tree_monoids(x, required = TRUE)
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
#' @examples
#' t <- tree_from(1:6)
#' t[c(2, 5)] <- list(20, 50)
#' sum_m <- MeasureMonoid(`+`, 0, as.numeric)
#' reduce_left(t, sum_m)
#'
#' # Replacement length must match
#' try(t[c(1, 2)] <- list(999))
#' @export
`[<-.FingerTree` <- function(x, i, value) {
  ms <- resolve_tree_monoids(x, required = TRUE)
  xs <- .ft_to_list(x)
  idx <- .ft_assert_int_indices(i, length(xs))
  vals <- as.list(value)
  if(length(vals) != length(idx)) {
    stop("Replacement length must match index length exactly.")
  }
  for(k in seq_along(idx)) {
    xs[[idx[[k]]]] <- vals[[k]]
  }
  tree_from(xs, monoids = ms)
}

#' Replace a single element by 1-based index
#'
#' @param x FingerTree.
#' @param i Positive scalar integer index.
#' @param value Replacement element.
#' @return A new FingerTree with one element replaced.
#' @examples
#' t <- tree_from(letters[1:4])
#' t[[2]] <- "ZZ"
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_left(t, cat_m)
#'
#' # Character index updates internal field (advanced/debug usage)
#' u <- tree_from(letters[1:3])
#' u[["prefix"]] <- u$prefix
#' @export
`[[<-.FingerTree` <- function(x, i, value) {
  if(is.character(i) && length(i) == 1L && !is.na(i)) {
    attrs <- attributes(x)
    y <- unclass(x)
    y[[i]] <- value
    attributes(y) <- attrs
    return(y)
  }
  ms <- resolve_tree_monoids(x, required = TRUE)
  xs <- .ft_to_list(x)
  idx <- .ft_assert_int_indices(i, length(xs))
  if(length(idx) != 1L) {
    stop("[[<- expects exactly one index.")
  }
  xs[[idx]] <- value
  tree_from(xs, monoids = ms)
}
