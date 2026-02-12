#' Append an element
#'
#' @param t FingerTree.
#' @param x Element to append.
#' @return Updated tree.
#'   If `t` is name-indexed, appending unnamed elements is invalid.
#' @examples
#' t <- tree_from(letters[1:3])
#' t2 <- append(t, "d")
#' t2[[4]]
#'
#' # Compose with prepend and reduce
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_right(prepend(t2, "z"), cat_m)
#'
#' # Append to a named tree with an explicit element name
#' tn <- tree_from(setNames(as.list(1:2), c("a", "b")))
#' tn2 <- append(tn, stats::setNames(3, "c"))
#' tn2[["c"]]
#' @export
# Runtime: O(log n) tree update, with O(1) local name-state checks.
append <- function(t, x) {
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

  x2 <- x
  if(nn == 0L) {
    # Fast unnamed path: avoid attr writes when incoming element is also unnamed.
    nm <- .ft_get_name(x)
    if(is.null(nm)) {
      nm <- .ft_name_from_value(x)
    }
    if(is.null(nm)) {
      if(.ft_cpp_can_use(ms)) {
        return(.ft_cpp_add_right(t, x2, ms))
      }
      return(.add_right_fast(t, x2, ms))
    }
    if(n > 0L) {
      stop("Cannot mix named and unnamed elements (append would create mixed named and unnamed tree).")
    }
    x2 <- .ft_set_name(x2, nm)
  } else {
    nm <- .ft_effective_name(x)
    if(is.null(nm)) {
      stop("Cannot mix named and unnamed elements (append would create mixed named and unnamed tree).")
    }
    x2 <- .ft_set_name(x2, nm)
  }

  if(.ft_cpp_can_use(ms)) {
    return(.ft_cpp_add_right(t, x2, ms))
  }
  .add_right_fast(t, x2, ms)
}
