#' Construct a split predicate wrapper
#'
#' @param f Predicate function over accumulated measure values.
#' @return The predicate function.
#' @export
Predicate <- function(f) {
  Function(f)
}
