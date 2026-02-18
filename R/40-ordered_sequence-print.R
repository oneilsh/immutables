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
  entries <- .oms_entries(x)[seq_len(k)]
  preview <- paste(
    vapply(entries, function(e) {
      key_txt <- paste(capture.output(str(e$key, give.attr = FALSE, vec.len = 1)), collapse = " ")
      item_txt <- paste(capture.output(str(e$item, give.attr = FALSE, vec.len = 3)), collapse = " ")
      sprintf("{key=%s item=%s}", key_txt, item_txt)
    }, character(1)),
    collapse = " | "
  )
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
