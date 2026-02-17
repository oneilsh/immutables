#' Apply with S3 dispatch
#'
#' `apply()` is exported as an S3 generic so immutable structures can provide
#' specialized behavior while preserving base array behavior by default.
#'
#' For immutable structures, use `apply(x, FUN, ...)` (no `MARGIN`):
#' - `flexseq`: `FUN(value, ...)` returns transformed value.
#' - `priority_queue`: `FUN(item, priority, name, ...)` returns a named
#'   list using fields from `item`, `priority`, `name`.
#' - `ordered_sequence`: `FUN(item, key, name, ...)` returns a named
#'   list using fields from `item`, `key`, `name`.
#'
#' @param X Object to apply over.
#' @param MARGIN For base arrays, margin(s) to operate over.
#' @param FUN Function to apply.
#' @param ... Method-specific arguments.
#' @param preserve_monoids Logical flag used by `apply.flexseq()`.
#' @return Method-dependent result.
#' @export
apply <- function(X, MARGIN, FUN, ...) {
  UseMethod("apply")
}

#' @export
#' @noRd
apply.default <- function(X, MARGIN, FUN, ...) {
  base::apply(X, MARGIN = MARGIN, FUN = FUN, ...)
}
