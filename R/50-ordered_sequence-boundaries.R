# Runtime: O(1).
.oms_stop_interval_index <- function(x, fn_name) {
  if(inherits(x, "interval_index")) {
    stop(sprintf("`%s()` is not supported for interval_index. Use interval query helpers (`peek_*`, `pop_*`, and `peek_point()`).", fn_name))
  }
  invisible(TRUE)
}

#' Ordered sequences cannot be concatenated with `c()`
#'
#' Rebuild with `ordered_sequence()` / `as_ordered_sequence()` if you need to
#' combine ordered keyed values.
#'
#' @method c ordered_sequence
#' @param ... Ordered objects.
#' @param recursive Unused.
#' @return Never returns; always errors.
#' @export
#' @noRd
# Runtime: O(1).
c.ordered_sequence <- function(..., recursive = FALSE) {
  xs <- list(...)
  target <- if(length(xs) == 0L) "ordered_sequence" else .ft_ordered_owner_class(xs[[1L]])
  stop(sprintf("`c()` is not supported for %s. Cast first with `as_flexseq()`.", target))
}
