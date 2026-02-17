#' Prepend an element
#'
#' @param t A `flexseq`.
#' @param x Element to prepend.
#' @return Updated tree.
#'   If `t` is name-indexed, prepending unnamed elements is invalid.
#' @examples
#' x <- as_flexseq(letters[2:5])
#' x
#'
#' x2 <- prepend(x, "a")
#' x2
#'
#' # prepending to a named sequence requires a named element
#' x3 <- as_flexseq(list(two = 2, three = 3))
#' new_el <- 1
#' names(new_el) <- "one"
#' x4 <- prepend(x3, new_el)
#' x4
#' @export
# Runtime: O(log n) tree update, with O(1) local name-state checks.
prepend <- function(t, x) {
  ms <- attr(t, "monoids", exact = TRUE)
  if(is.null(ms)) {
    stop("Tree has no monoids attribute.")
  }
  m <- attr(t, "measures", exact = TRUE)
  if(is.null(m)) {
    stop("Tree has no measures attribute.")
  }
  n <- as.integer(m[[".size"]])
  nn <- as.integer(m[[".named_count"]])
  # Runtime: O(1). Enforce non-mixed naming invariant without full traversal.
  if(n > 0L && nn != 0L && nn != n) {
    stop("Invalid tree name state: mixed named/unnamed elements.")
  }

  x2 <- if(inherits(t, "priority_queue")) {
    .pq_parse_entry(x, context = "prepend() on priority_queue")
  } else {
    x
  }
  if(nn == 0L) {
    # Fast unnamed path: avoid attr writes when incoming element is also unnamed.
    nm <- .ft_get_name(x2)
    if(is.null(nm)) {
      nm <- .ft_name_from_value(x2)
    }
    if(is.null(nm)) {
      if(.ft_cpp_can_use(ms)) {
        return(.ft_restore_subclass(.ft_cpp_add_left(t, x2, ms), t, context = "prepend()"))
      }
      return(.ft_restore_subclass(.add_left_fast(t, x2, ms), t, context = "prepend()"))
    }
    if(n > 0L) {
      stop("Cannot mix named and unnamed elements (prepend would create mixed named and unnamed tree).")
    }
    if(.ft_cpp_can_use(ms)) {
      return(.ft_restore_subclass(.ft_cpp_add_left_named(t, x2, nm, ms), t, context = "prepend()"))
    }
    x2 <- .ft_set_name(x2, nm)
  } else {
    nm <- .ft_effective_name(x2)
    if(is.null(nm)) {
      stop("Cannot mix named and unnamed elements (prepend would create mixed named and unnamed tree).")
    }
    if(.ft_cpp_can_use(ms)) {
      return(.ft_restore_subclass(.ft_cpp_add_left_named(t, x2, nm, ms), t, context = "prepend()"))
    }
    x2 <- .ft_set_name(x2, nm)
  }

  if(.ft_cpp_can_use(ms)) {
    return(.ft_restore_subclass(.ft_cpp_add_left(t, x2, ms), t, context = "prepend()"))
  }
  .ft_restore_subclass(.add_left_fast(t, x2, ms), t, context = "prepend()")
}
