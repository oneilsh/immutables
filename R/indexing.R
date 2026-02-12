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
  n <- as.integer(node_measure(x, ".size"))
  idx <- .ft_assert_int_indices(i, n)
  if(length(idx) == 0L) {
    return(empty_tree(monoids = ms))
  }
  out <- lapply(idx, function(j) {
    hit <- locate(x, function(v) v >= j, ".size")
    if(!isTRUE(hit$found)) {
      stop("Index out of bounds.")
    }
    hit$elem
  })
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
  ms <- resolve_tree_monoids(x, required = TRUE)
  n <- as.integer(node_measure(x, ".size"))
  idx <- .ft_assert_int_indices(i, n)
  if(length(idx) != 1L) {
    stop("[[ expects exactly one index.")
  }

  hit <- locate(x, function(v) v >= idx, ".size")
  if(!isTRUE(hit$found)) {
    stop("Index out of bounds.")
  }
  hit$elem
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
  resolve_tree_monoids(x, required = TRUE)
  n <- as.integer(node_measure(x, ".size"))
  idx <- .ft_assert_int_indices(i, n)
  vals <- as.list(value)
  if(length(vals) != length(idx)) {
    stop("Replacement length must match index length exactly.")
  }

  out <- x
  for(k in seq_along(idx)) {
    out[[idx[[k]]]] <- vals[[k]]
  }
  assert_structural_attrs(out)
  out
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
  n <- as.integer(node_measure(x, ".size"))
  idx <- .ft_assert_int_indices(i, n)
  if(length(idx) != 1L) {
    stop("[[<- expects exactly one index.")
  }

  s <- split_tree(x, function(v) v >= idx, ".size")
  left_plus <- append(s$left, value)
  out <- concat(left_plus, s$right, ms)
  assert_structural_attrs(out)
  out
}
