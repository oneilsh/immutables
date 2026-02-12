# compact one-line formatter for preview elements.
.ft_format_preview_elem(x, width) %::% . : integer : character
.ft_format_preview_elem(x, width = 40L) %as% {
  if(is.null(x)) {
    return("<NULL>")
  }
  if(length(x) == 1L && (is.character(x) || is.numeric(x) || is.integer(x) || is.logical(x))) {
    return(as.character(x))
  }
  txt <- paste(deparse(x, width.cutoff = 120L), collapse = " ")
  if(nchar(txt, type = "chars") > width) {
    paste0(substr(txt, 1L, width - 3L), "...")
  } else {
    txt
  }
}

#' Print a compact summary of a finger tree
#'
#' @method print FingerTree
#' @param x FingerTree.
#' @param show_internal_monoids Logical; show internal monoids (`.size`,
#'   `.named_count`). Default `FALSE`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @examples
#' t <- tree_from(letters[1:10])
#' t
#'
#' tn <- tree_from(setNames(as.list(1:5), paste0("k", 1:5)))
#' tn
#' @export
print.FingerTree <- function(x, show_internal_monoids = FALSE, ...) {
  n <- as.integer(node_measure(x, ".size"))
  nn <- as.integer(node_measure(x, ".named_count"))
  named_state <- if(nn == 0L) "none" else "all"
  monoids <- names(resolve_tree_monoids(x, required = TRUE))
  visible_monoids <- if(isTRUE(show_internal_monoids)) {
    monoids
  } else {
    setdiff(monoids, c(".size", ".named_count"))
  }

  vals_raw <- .ft_to_list(x)
  vals <- lapply(vals_raw, .ft_strip_name)
  entries <- if(named_state == "all" && n > 0L) {
    vapply(seq_along(vals_raw), function(k) {
      nm <- .ft_get_name(vals_raw[[k]])
      paste0(nm, " = ", .ft_format_preview_elem(vals[[k]], width = 40L))
    }, character(1))
  } else if(n > 0L) {
    vapply(vals, .ft_format_preview_elem, character(1), width = 40L)
  } else {
    character(0)
  }

  if(n == 0L) {
    preview <- character(0)
  } else if(n <= 8L) {
    preview <- entries
  } else {
    left <- entries[1:4]
    right <- entries[(n - 3):n]
    preview <- c(left, "...", right)
  }

  cat("FingerTree <", "size=", n, ", named=", named_state, ">\n", sep = "")
  cat("  monoids: ", if(length(visible_monoids) == 0L) "none" else paste(visible_monoids, collapse = ", "), "\n", sep = "")
  cat("  preview: ", if(length(preview) == 0L) "<empty>" else paste(preview, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @rdname print.FingerTree
#' @method print Deep
#' @export
print.Deep <- function(x, ...) {
  print.FingerTree(x, ...)
}

#' @rdname print.FingerTree
#' @method print Single
#' @export
print.Single <- function(x, ...) {
  print.FingerTree(x, ...)
}

#' @rdname print.FingerTree
#' @method print Empty
#' @export
print.Empty <- function(x, ...) {
  print.FingerTree(x, ...)
}
