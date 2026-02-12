#' Prepend an element
#'
#' @param t FingerTree.
#' @param x Element to prepend.
#' @return Updated tree.
#'   If `t` is name-indexed, prepending unnamed elements is invalid.
#' @examples
#' t <- tree_from(letters[2:4])
#' t2 <- prepend(t, "a")
#' t2[[1]]
#'
#' # Compose with append and reduce
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_left(append(t2, "z"), cat_m)
#'
#' # Prepend to a named tree with an explicit element name
#' tn <- tree_from(setNames(as.list(2:3), c("b", "c")))
#' tn2 <- prepend(tn, stats::setNames(1, "a"))
#' tn2[["a"]]
#' @export
# Runtime: O(log n) tree update, with O(1) local name-state checks.
prepend <- function(t, x) {
  ms <- attr(t, "monoids", exact = TRUE)
  if(is.null(ms)) {
    stop("Tree has no monoids attribute.")
  }
  x2 <- .ft_set_name(x, .ft_effective_name(x))
  m <- attr(t, "measures", exact = TRUE)
  if(is.null(m)) {
    stop("Tree has no measures attribute.")
  }
  n <- as.integer(m[[".size"]])
  nn <- as.integer(m[[".named_count"]])
  x_named <- !is.null(.ft_get_name(x2))
  # Runtime: O(1). Enforce non-mixed naming invariant without full traversal.
  if(n > 0L) {
    if(nn != 0L && nn != n) {
      stop("Invalid tree name state: mixed named/unnamed elements.")
    }
    if(nn == 0L && x_named) {
      stop("Cannot mix named and unnamed elements (prepend would create mixed named and unnamed tree).")
    }
    if(nn == n && !x_named) {
      stop("Cannot mix named and unnamed elements (prepend would create mixed named and unnamed tree).")
    }
  }
  if(.ft_cpp_can_use(ms)) {
    return(.ft_cpp_add_left(t, x2, ms))
  }
  add_left(t, x2, ms)
}
