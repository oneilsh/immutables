#' Print an interval index summary
#'
#' @param x An `interval_index`.
#' @param max_elements Maximum number of elements shown in preview.
#' @param ... Unused.
#' @return Invisibly returns `x`.
#' @export
#' @method print interval_index
# Runtime: O(min(n, max_elements)).
print.interval_index <- function(x, max_elements = 8L, ...) {
  .ivx_assert_index(x)

  n <- length(x)
  endpoint_type <- .ivx_endpoint_type_state(x)
  bounds <- .ivx_bounds_state(x)

  cat(sprintf(
    "<interval_index> size=%d endpoint_type=%s bounds=%s\n",
    n,
    if(is.null(endpoint_type)) "<unset>" else endpoint_type,
    bounds
  ))

  if(n == 0L) {
    cat("  preview: []\n")
    return(invisible(x))
  }

  k <- min(as.integer(max_elements), n)
  entries <- .ivx_entries(x)[seq_len(k)]
  preview <- paste(
    vapply(entries, function(e) {
      start_txt <- paste(capture.output(str(e$start, give.attr = FALSE, vec.len = 1)), collapse = " ")
      end_txt <- paste(capture.output(str(e$end, give.attr = FALSE, vec.len = 1)), collapse = " ")
      item_txt <- paste(capture.output(str(e$item, give.attr = FALSE, vec.len = 3)), collapse = " ")
      sprintf("{start=%s end=%s item=%s}", start_txt, end_txt, item_txt)
    }, character(1)),
    collapse = " | "
  )

  cat(sprintf("  preview[%d]: %s\n", k, preview))
  invisible(x)
}
