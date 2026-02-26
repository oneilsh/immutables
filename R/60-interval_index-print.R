# Runtime: O(1).
.ivx_bounds_phrase <- function(bounds) {
  paste0(substr(bounds, 1L, 1L), "start, end", substr(bounds, 2L, 2L))
}

#' Print an interval index summary
#'
#' @param x An `interval_index`.
#' @param max_elements Maximum number of elements shown in preview (`head + tail`).
#' @param ... Passed through to per-element `print()`.
#' @return Invisibly returns `x`.
#' @examples
#' ix <- interval_index(
#'   one = 1, two = 2, three = 3,
#'   start = c(20, 30, 10), end = c(25, 37, 24)
#' )
#' print(ix, max_elements = 4)
#'
#' ix2 <- interval_index(1, 2, 3, start = c(2, 4, 6), end = c(3, 5, 8), bounds = "[]")
#' print(ix2, max_elements = 3)
#' @export
#' @method print interval_index
# Runtime: O((k + h) log n), where k = shown elements and h = preview split overhead.
print.interval_index <- function(x, max_elements = 4L, ...) {
  .ivx_assert_index(x)
  max_elements <- .ft_validate_print_max_elements(max_elements)

  n <- length(x)
  bounds <- .ivx_resolve_bounds(x, NULL)
  nn <- as.integer(node_measure(x, ".named_count"))
  named <- isTRUE(nn > 0L)

  cat(
    if(named) "Named" else "Unnamed",
    " interval_index with ",
    n,
    " element",
    if(n == 1L) "" else "s",
    ", default bounds ",
    .ivx_bounds_phrase(bounds),
    ".\n",
    sep = ""
  )

  if(n == 0L || max_elements == 0L) {
    return(invisible(x))
  }

  cat("\nElements (by interval start order):\n\n")
  preview <- .pick_preview_sizes(n, max_elements)
  for(i in preview$head) {
    entry <- .ft_get_elem_at(x, as.integer(i))
    nm <- .ft_get_name(entry)
    iv <- paste0(substr(bounds, 1L, 1L), .ft_format_scalar(entry$start), ", ", .ft_format_scalar(entry$end), substr(bounds, 2L, 2L))
    if(named && !is.null(nm)) {
      cat("$", nm, " (interval ", iv, ")\n", sep = "")
    } else {
      cat("[[", i, "]] (interval ", iv, ")\n", sep = "")
    }
    print(entry$item, ...)
    cat("\n")
  }

  .ft_print_skipped(preview$skipped)

  for(i in preview$tail) {
    entry <- .ft_get_elem_at(x, as.integer(i))
    nm <- .ft_get_name(entry)
    iv <- paste0(substr(bounds, 1L, 1L), .ft_format_scalar(entry$start), ", ", .ft_format_scalar(entry$end), substr(bounds, 2L, 2L))
    if(named && !is.null(nm)) {
      cat("$", nm, " (interval ", iv, ")\n", sep = "")
    } else {
      cat("[[", i, "]] (interval ", iv, ")\n", sep = "")
    }
    print(entry$item, ...)
    cat("\n")
  }
  invisible(x)
}
