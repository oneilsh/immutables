#SO

#' Fapply with S3 dispatch
#'
#' `fapply()` is an S3 generic for applying functions over immutable
#' structures with type-specific dispatch.
#'
#' @param X Object to apply over.
#' @param FUN Function to apply.
#' @param ... Method-specific arguments.
#' @return Method-dependent result.
#' @seealso [fapply.flexseq()], [fapply.priority_queue()], [fapply.ordered_sequence()], [fapply.interval_index()]
#' @export
fapply <- function(X, FUN, ...) {
  UseMethod("fapply")
}
