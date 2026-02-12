# collect first k elements in tree order without flattening full tree.
# Runtime: O(k + h), where h is traversed structural overhead until k elements.
.ft_preview_fill(x, st) %::% . : environment : .
.ft_preview_fill(x, st) %as% {
  if(st$pos > st$k) {
    return(invisible(NULL))
  }
  if(!is_structural_node(x)) {
    st$out[[st$pos]] <- x
    st$pos <- st$pos + 1L
    return(invisible(NULL))
  }
  if(x %isa% Empty) {
    return(invisible(NULL))
  }
  if(x %isa% Single) {
    .ft_preview_fill(.subset2(x, 1), st)
    return(invisible(NULL))
  }
  if(x %isa% Deep) {
    .ft_preview_fill(.subset2(x, "prefix"), st)
    .ft_preview_fill(.subset2(x, "middle"), st)
    .ft_preview_fill(.subset2(x, "suffix"), st)
    return(invisible(NULL))
  }
  for(el in x) {
    .ft_preview_fill(el, st)
    if(st$pos > st$k) {
      break
    }
  }
  invisible(NULL)
}

# take first k elements from a tree without materializing all elements.
# Runtime: O(k + h), where h is traversed structural overhead until k elements.
.ft_preview_first_k(x, k) %::% . : integer : list
.ft_preview_first_k(x, k) %as% {
  if(k <= 0L) {
    return(list())
  }
  st <- new.env(parent = emptyenv())
  st$k <- k
  st$pos <- 1L
  st$out <- vector("list", k)
  .ft_preview_fill(x, st)
  used <- st$pos - 1L
  if(used <= 0L) {
    return(list())
  }
  st$out[seq_len(used)]
}

#' Print a compact summary of a finger tree
#'
#' @method print FingerTree
#' @param x FingerTree.
#' @param max_elements Maximum number of elements to show in list-style preview.
#'   Default `6`.
#' @param show_internal_monoids Logical; show internal monoids (`.size`,
#'   `.named_count`). Default `FALSE`.
#' @param ... Passed through to `print()` for preview elements.
#' @return `x`, invisibly.
#' @examples
#' t <- tree_from(letters[1:10])
#' t
#'
#' tn <- tree_from(setNames(as.list(1:5), paste0("k", 1:5)))
#' tn
#' @export
# Runtime: O(min(n, max_elements) + h) for preview extraction where h is
# traversed structural overhead; avoids full-tree flattening.
print.FingerTree <- function(x, max_elements = 6L, show_internal_monoids = FALSE, ...) {
  n <- as.integer(node_measure(x, ".size"))
  nn <- as.integer(node_measure(x, ".named_count"))
  named <- if(nn == 0L) "no" else "yes"
  monoids <- names(resolve_tree_monoids(x, required = TRUE))
  visible_monoids <- if(isTRUE(show_internal_monoids)) {
    monoids
  } else {
    setdiff(monoids, c(".size", ".named_count"))
  }

  max_elements <- as.integer(max_elements)
  if(length(max_elements) != 1L || is.na(max_elements) || max_elements < 0L) {
    stop("`max_elements` must be a single non-negative integer.")
  }

  vals_raw <- .ft_preview_first_k(x, min(n, max_elements))
  vals <- lapply(vals_raw, .ft_strip_name)
  if(named == "yes" && n > 0L) {
    names(vals) <- vapply(vals_raw, .ft_get_name, character(1))
  }

  shown <- min(n, max_elements)
  preview <- vals[seq_len(shown)]
  hidden <- n - shown

  cat("FingerTree <", "size=", n, ", named=", named, ">\n", sep = "")
  cat("  monoids: ", if(length(visible_monoids) == 0L) "none" else paste(visible_monoids, collapse = ", "), "\n\n", sep = "")
  if(shown == 0L) {
    cat("  preview: <empty>\n")
  } else {
    print(preview, ...)
    if(hidden > 0L) {
      cat("... and ", hidden, " more element", if(hidden == 1L) "" else "s", " not shown\n", sep = "")
    }
  }
  invisible(x)
}

#' @rdname print.FingerTree
#' @method print Deep
#' @export
# Runtime: Delegates to `print.FingerTree`.
print.Deep <- function(x, ...) {
  print.FingerTree(x, ...)
}

#' @rdname print.FingerTree
#' @method print Single
#' @export
# Runtime: Delegates to `print.FingerTree`.
print.Single <- function(x, ...) {
  print.FingerTree(x, ...)
}

#' @rdname print.FingerTree
#' @method print Empty
#' @export
# Runtime: Delegates to `print.FingerTree`.
print.Empty <- function(x, ...) {
  print.FingerTree(x, ...)
}
