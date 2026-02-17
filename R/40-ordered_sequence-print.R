#' Print an ordered sequence summary
#'
#' @param x An `ordered_sequence`.
#' @param max_elements Maximum number of elements shown in preview.
#' @param ... Unused.
#' @return Invisibly returns `x`.
#' @export
#' @method print ordered_sequence
# Runtime: O(min(n, max_elements)).
print.ordered_sequence <- function(x, max_elements = 8L, ...) {
  .oms_assert_set(x)

  n <- length(x)
  key_type <- .oms_key_type_state(x)
  cat(sprintf("<ordered_sequence> size=%d key_type=%s\n", n, if(is.null(key_type)) "<unset>" else key_type))

  if(n == 0L) {
    cat("  preview: []\n")
    return(invisible(x))
  }

  k <- min(as.integer(max_elements), n)
  vals <- as.list.ordered_sequence(x)[seq_len(k)]
  preview <- paste(vapply(vals, function(v) paste(capture.output(str(v, give.attr = FALSE, vec.len = 3)), collapse = " "), character(1)), collapse = " | ")
  cat(sprintf("  preview[%d]: %s\n", k, preview))
  invisible(x)
}

#' Print an ordered multiset summary
#'
#' @param x An `ordered_multiset`.
#' @param max_elements Maximum number of elements shown in preview.
#' @param ... Unused.
#' @return Invisibly returns `x`.
#' @export
#' @method print ordered_multiset
# Runtime: O(min(n, max_elements)).
print.ordered_multiset <- function(x, max_elements = 8L, ...) {
  .oms_assert_multiset(x)
  NextMethod("print")
}
