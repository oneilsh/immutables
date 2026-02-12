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
Predicate <- function(f) {
  Function(f)
}
