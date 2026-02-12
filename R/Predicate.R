#' Construct a split predicate wrapper
#'
#' @param f Predicate function over accumulated measure values.
#' @return The predicate function.
#' @examples
#' p <- Predicate(function(v) v >= 3)
#' p(2)
#' p(3)
#'
#' t <- tree_from(letters[1:5])
#' locate(t, p, ".size")
#' @export
# Runtime: O(1).
Predicate <- function(f) {
  if(!is.function(f)) {
    stop("`f` must be a function.")
  }
  f
}
