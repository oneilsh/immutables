#' Print an ordered sequence summary
#'
#' @param x An `ordered_sequence`.
#' @param max_elements Maximum number of elements shown in preview (`head + tail`).
#' @param ... Passed through to per-element `print()`.
#' @return Invisibly returns `x`.
#' @examples
#' xs <- ordered_sequence(one = 1, two = 2, three = 3, keys = c(20, 30, 10))
#' print(xs, max_elements = 4)
#'
#' ys <- ordered_sequence(1, 2, 3, keys = c(2, 1, 3))
#' print(ys, max_elements = 3)
#' @export
#' @method print ordered_sequence
# Runtime: O((k + h) log n), where k = shown elements and h = preview split overhead.
print.ordered_sequence <- function(x, max_elements = 4L, ...) {
  .oms_assert_set(x)
  max_elements <- .ft_validate_print_max_elements(max_elements)

  n <- length(x)
  nn <- as.integer(node_measure(x, ".named_count"))
  named <- isTRUE(nn > 0L)
  cat(if(named) "Named" else "Unnamed", " ordered_sequence with ", n, " element", if(n == 1L) "" else "s", ".\n", sep = "")

  if(n == 0L || max_elements == 0L) {
    return(invisible(x))
  }

  cat("\nElements (by key order):\n\n")
  preview <- .pick_preview_sizes(n, max_elements)
  for(i in preview$head) {
    entry <- .ft_get_elem_at(x, as.integer(i))
    nm <- .ft_get_name(entry)
    if(named && !is.null(nm)) {
      cat("$", nm, " (key ", .ft_format_scalar(entry$key), ")\n", sep = "")
    } else {
      cat("[[", i, "]] (key ", .ft_format_scalar(entry$key), ")\n", sep = "")
    }
    print(entry$item, ...)
    cat("\n")
  }

  .ft_print_skipped(preview$skipped)

  for(i in preview$tail) {
    entry <- .ft_get_elem_at(x, as.integer(i))
    nm <- .ft_get_name(entry)
    if(named && !is.null(nm)) {
      cat("$", nm, " (key ", .ft_format_scalar(entry$key), ")\n", sep = "")
    } else {
      cat("[[", i, "]] (key ", .ft_format_scalar(entry$key), ")\n", sep = "")
    }
    print(entry$item, ...)
    cat("\n")
  }
  invisible(x)
}
