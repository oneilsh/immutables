# mark a structural tree as a user-facing flexseq object.
# Runtime: O(1).
.as_flexseq <- function(x) {
  if(!is_structural_node(x)) {
    stop("Expected a structural tree node.")
  }
  class(x) <- unique(c("flexseq", setdiff(class(x), "list")))
  x
}

# normalize constructor input to list without dropping names.
# Runtime: O(n), where n is number of constructor inputs.
.flexseq_input_list <- function(x) {
  if(is.list(x)) {
    return(x)
  }
  as.list(x)
}

#' Construct a Persistent Flexible Sequence
#'
#' Works like `list(...)`, but returns an immutable sequence backed by
#' measured finger-tree internals.
#'
#' @param ... Sequence elements.
#' @param monoids Optional named list of measure monoids.
#' @return A `flexseq` object.
#' @examples
#' x <- flexseq(1, 2, 3)
#' x
#'
#' x2 <- flexseq("a", "b", "c")
#' x2
#' @export
# Runtime: O(n log n) over element count.
flexseq <- function(..., monoids = NULL) {
  args <- list(...)
  as_flexseq(args, monoids = monoids)
}

#' Coerce to a Persistent Flexible Sequence
#'
#' @param x Input vector/list-like object.
#' @param monoids Optional named list of measure monoids.
#' @return A `flexseq` object.
#' @examples
#' x <- as_flexseq(1:5)
#' x
#'
#' x2 <- as_flexseq(list(one = 1, two = 2, three = 3))
#' x2
#' @export
# Runtime: O(n log n) over element count.
as_flexseq <- function(x, monoids = NULL) {
  t <- tree_from(x, monoids = monoids)
  .as_flexseq(t)
}

#' Concatenate Sequences
#'
#' @method c flexseq
#' @param ... `flexseq` objects.
#' @param recursive Unused; must be `FALSE`.
#' @return A concatenated `flexseq`.
#' @export
# Runtime: O(sum(n_i)) worst-case with monoid harmonization.
c.flexseq <- function(..., recursive = FALSE) {
  if(isTRUE(recursive)) {
    stop("`recursive = TRUE` is not supported for flexseq.")
  }
  xs <- list(...)
  if(length(xs) == 0L) {
    return(flexseq())
  }
  out <- xs[[1]]
  for(i in 2:length(xs)) {
    out <- concat_trees(out, xs[[i]])
  }
  .as_flexseq(out)
}

#' Plot a Sequence Tree
#'
#' @method plot flexseq
#' @param x A `flexseq`.
#' @param ... Passed to the internal tree plotting routine.
#' @export
# Runtime: O(n) to build plot graph data.
plot.flexseq <- function(x, ...) {
  plot_tree(x, ...)
}

#' Sequence Length
#'
#' @method length flexseq
#' @param x A `flexseq`.
#' @return Number of elements in the sequence.
#' @export
# Runtime: O(1) using cached `.size` measure.
length.flexseq <- function(x) {
  as.integer(node_measure(x, ".size"))
}

#' Coerce a Sequence to Base List
#'
#' Returns elements in left-to-right sequence order.
#'
#' @method as.list flexseq
#' @param x A `flexseq`.
#' @param ... Unused.
#' @return A base R list of sequence elements.
#' @export
# Runtime: O(n) over number of elements.
as.list.flexseq <- function(x, ...) {
  els <- .ft_to_list(x)
  n <- length(els)
  if(n == 0L) {
    return(list())
  }

  out <- vector("list", n)
  nms <- character(n)
  has_names <- FALSE
  for(i in seq_len(n)) {
    el <- els[[i]]
    out[[i]] <- .ft_strip_name(el)
    nm <- .ft_get_name(el)
    if(is.null(nm)) {
      nms[[i]] <- ""
    } else {
      nms[[i]] <- nm
      has_names <- TRUE
    }
  }
  if(has_names) {
    names(out) <- nms
  }
  out
}
