#' Check emptiness with S3 dispatch
#'
#' Generic `is_empty()` dispatches by class.
#'
#' @param x Object to check.
#' @param ... Reserved for method-specific arguments.
#' @return Logical scalar.
#' @examples
#' is_empty(flexseq())
#' is_empty(as_flexseq(1:3))
#'
#' is_empty(priority_queue())
#'
#' ms <- as_ordered_multiset(list("a"), keys = 1)
#' is_empty(ms)
#' @export
is_empty <- function(x, ...) {
  UseMethod("is_empty")
}

#' @export
#' @noRd
is_empty.default <- function(x, ...) {
  cls <- class(x)
  cls_txt <- if(length(cls) == 0L) "unknown" else paste(cls, collapse = "/")
  stop(sprintf("No `is_empty()` method for class '%s'.", cls_txt))
}
