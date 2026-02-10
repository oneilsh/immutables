resolve_tree_monoid <- function(t, monoid = NULL, required = FALSE) {
  if(!is.null(monoid)) {
    return(monoid)
  }
  r <- attr(t, "monoid")
  if(required && is.null(r)) {
    stop("No monoid provided and tree has no monoid attribute.")
  }
  r
}

set_tree_monoid <- function(t, monoid) {
  attr(t, "monoid") <- monoid
  t
}

#' Create an empty finger tree
#'
#' @param monoid Optional monoid stored on the tree for later use.
#' @return An empty finger tree.
#' @examples
#' r <- Monoid(function(a, b) a + b, 0)
#' t <- empty_tree(monoid = r)
#' @export
empty_tree <- function(monoid = NULL) {
  t <- Empty()
  if(!is.null(monoid)) {
    attr(t, "monoid") <- monoid
    if(is_measure_monoid(monoid)) {
      t <- measured_empty(monoid)
      attr(t, "monoid") <- monoid
    }
  }
  t
}

#' Build a tree from a vector or list
#'
#' @param x Elements to insert.
#' @param values Optional parallel values (same length as x). Stored as the
#'   `value` attribute on each element when provided.
#' @param monoid Optional monoid stored on the tree for later use.
#' @return A finger tree containing the elements.
#' @examples
#' t <- tree_from(1:3)
#' @export
tree_from <- function(x, values = NULL, monoid = NULL) {
  if(is_measure_monoid(monoid)) {
    t <- empty_tree(monoid)
    x_list <- as.list(x)
    if(is.null(values)) {
      for(el in x_list) {
        t <- append(t, el, monoid)
      }
    } else {
      v_list <- as.list(values)
      if(length(x_list) != length(v_list)) {
        stop("length of entries and values lists given to tree_from not equal.")
      }
      for(i in seq_along(x_list)) {
        el <- x_list[[i]]
        attr(el, "value") <- v_list[[i]]
        t <- append(t, el, monoid)
      }
    }
  } else {
    t <- if(is.null(values)) {
      as.FingerTree(x)
    } else {
      as.FingerTree(x, values)
    }
  }
  if(!is.null(monoid)) {
    attr(t, "monoid") <- monoid
  }
  t
}

#' Prepend an element
#'
#' @param t FingerTree.
#' @param x Element to prepend.
#' @param monoid Optional monoid to store on the tree.
#' @return Updated tree.
#' @export
prepend <- function(t, x, monoid = NULL) {
  r <- resolve_tree_monoid(t, monoid, required = FALSE)
  t2 <- if(is_measure_monoid(r)) add_left(t, x, r) else add_left(t, x)
  if(!is.null(r)) { attr(t2, "monoid") <- r }
  t2
}

#' Append an element
#'
#' @param t FingerTree.
#' @param x Element to append.
#' @param monoid Optional monoid to store on the tree.
#' @return Updated tree.
#' @export
append <- function(t, x, monoid = NULL) {
  r <- resolve_tree_monoid(t, monoid, required = FALSE)
  t2 <- if(is_measure_monoid(r)) add_right(t, x, r) else add_right(t, x)
  if(!is.null(r)) { attr(t2, "monoid") <- r }
  t2
}

#' Concatenate two trees
#'
#' @param x Left tree.
#' @param y Right tree.
#' @param monoid Optional monoid to store on the result.
#' @return Concatenated tree.
#' @export
concat_trees <- function(x, y, monoid = NULL) {
  r <- resolve_tree_monoid(x, monoid, required = FALSE)
  t <- if(is_measure_monoid(r)) concat(x, y, r) else concat(x, y)
  if(!is.null(r)) { attr(t, "monoid") <- r }
  t
}

#' Reduce from the left
#'
#' @param t FingerTree.
#' @param monoid Optional monoid; falls back to tree attribute.
#' @return Reduced value.
#' @export
reduce_left <- function(t, monoid = NULL) {
  r <- resolve_tree_monoid(t, monoid, required = TRUE)
  reduce_left_impl(t, r)
}

#' Reduce from the right
#'
#' @param t FingerTree.
#' @param monoid Optional monoid; falls back to tree attribute.
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
#' @param monoid Optional monoid; falls back to tree attribute.
#' @param accumulator Optional starting measure (defaults to monoid identity).
#' @return A list with `left`, `elem`, and `right`.
#' @export
split_tree <- function(t, predicate, monoid = NULL, accumulator = NULL) {
  r <- resolve_tree_monoid(t, monoid, required = TRUE)
  if(!is_measure_monoid(r)) {
    stop("split_tree requires a MeasureMonoid.")
  }
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
#' @param monoid Optional monoid; falls back to tree attribute.
#' @return A list with `left` and `right`.
#' @export
split <- function(t, predicate, monoid = NULL) {
  r <- resolve_tree_monoid(t, monoid, required = TRUE)
  if(!is_measure_monoid(r)) {
    stop("split requires a MeasureMonoid.")
  }

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
