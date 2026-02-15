#' Construct a Predicate Function
#'
#' @param f Predicate function over accumulated measure values.
#' @return The predicate function.
#' @examples
#' p <- predicate(function(v) v >= 3)
#' p(2)
#' p(3)
#'
#' t <- as_flexseq(letters[1:5])
#' locate(t, p, ".size")
#' @export
# Runtime: O(1).
predicate <- function(f) {
  if(!is.function(f)) {
    stop("`f` must be a function.")
  }
  class(f) <- unique(c("predicate", class(f)))
  f
}
