#' Apply a Function to Each Sequence Element
#'
#' Maps `f` over elements of a `flexseq` and returns a new `flexseq`.
#' Traversal is linear, and rebuild uses standard tree construction.
#'
#' @param x A `flexseq`.
#' @param f Function applied to each element.
#' @param preserve_monoids Logical; when `TRUE`, carries all input monoids to
#'   the output. When `FALSE` (default), output keeps only invariant monoids
#'   (`.size`, `.named_count`).
#' @param ... Additional arguments passed to `f`.
#' @return A `flexseq` with transformed elements.
#' @examples
#' x <- as_flexseq(1:5)
#' seq_apply(x, function(v) v * 10)
#'
#' xn <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
#' seq_apply(xn, function(v) v + 1)
#' @export
# Runtime: O(n log n) total (O(n) traversal + O(n log n) rebuild).
seq_apply <- function(x, f, preserve_monoids = FALSE, ...) {
  if(!inherits(x, "flexseq")) {
    stop("`x` must be a flexseq.")
  }
  if(!is.function(f)) {
    stop("`f` must be a function.")
  }
  if(!is.logical(preserve_monoids) || length(preserve_monoids) != 1L || is.na(preserve_monoids)) {
    stop("`preserve_monoids` must be TRUE or FALSE.")
  }

  els <- .ft_to_list(x)
  n <- length(els)
  out <- vector("list", n)

  for(i in seq_len(n)) {
    out[[i]] <- f(.ft_strip_name(els[[i]]), ...)
  }

  if(n > 0L) {
    el_names <- vapply(els, function(el) {
      nm <- .ft_get_name(el)
      if(is.null(nm)) NA_character_ else nm
    }, character(1))
    has_named <- any(!is.na(el_names))
    if(has_named) {
      if(any(is.na(el_names))) {
        stop("Invalid name state: mixed named and unnamed elements are not allowed.")
      }
      names(out) <- el_names
    }
  }

  out_monoids <- if(isTRUE(preserve_monoids)) {
    attr(x, "monoids", exact = TRUE)
  } else {
    ensure_size_monoids(list(.size = size_measure_monoid()))
  }

  as_flexseq(out, monoids = out_monoids)
}
