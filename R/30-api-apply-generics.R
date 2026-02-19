#' Lapply with S3 dispatch
#'
#' `lapply()` is exported as an S3 generic so immutable structures can provide
#' specialized behavior while preserving base list behavior by default.
#'
#' For immutable structures, use `lapply(x, FUN, ...)`:
#' - `flexseq`: `FUN(value, ...)` returns transformed value.
#' - `priority_queue`: `FUN(item, priority, name, ...)` returns a named
#'   list using fields from `item`, `priority`, `name`.
#' - `ordered_sequence`: `FUN(item, key, name, ...)` returns a named
#'   list using fields from `item`, `key`, `name`.
#'
#' @param X Object to apply over.
#' @param FUN Function to apply.
#' @param ... Method-specific arguments.
#' @param preserve_monoids Logical flag used by `lapply.flexseq()`.
#' @return Method-dependent result.
#' @export
lapply <- function(X, FUN, ...) {
  UseMethod("lapply")
}

#' @export
#' @noRd
lapply.default <- function(X, FUN, ...) {
  base::lapply(X, FUN = FUN, ...)
}
