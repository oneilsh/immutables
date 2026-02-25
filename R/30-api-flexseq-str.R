#SO

#' Display Internal Structure of a flexseq
#'
#' @method str flexseq
#' @param object A `flexseq`.
#' @param ... Passed to [utils::str()].
#' @return `NULL`, invisibly.
#' @keywords internal
#' @export
str.flexseq <- function(object, ...) {
  utils::str(unclass(object), ...)
  invisible(NULL)
}
