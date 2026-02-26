#SO

# Runtime: O(n) total (O(n) traversal + O(n) linear rebuild).
.seq_apply_impl <- function(x, f, ..., preserve_custom_monoids = TRUE) {
  if(!inherits(x, "flexseq")) {
    stop("`x` must be a flexseq.")
  }
  if(!is.function(f)) {
    stop("`f` must be a function.")
  }
  if(!is.logical(preserve_custom_monoids) || length(preserve_custom_monoids) != 1L || is.na(preserve_custom_monoids)) {
    stop("`preserve_custom_monoids` must be TRUE or FALSE.")
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

  out_monoids <- if(isTRUE(preserve_custom_monoids)) {
    resolve_tree_monoids(x, required = TRUE)
  } else {
    ensure_size_monoids(list(.size = size_measure_monoid()))
  }

  .as_flexseq_build(out, monoids = out_monoids)
}

#' Apply a function over flexseq elements
#'
#' @method fapply flexseq
#' @param X A `flexseq`.
#' @param FUN Function to apply to each element.
#' @param preserve_custom_monoids Logical scalar. If `TRUE` (default), rebuild with the
#'   full current monoid set (including user monoids). If `FALSE`, rebuild using
#'   only required structural monoids (`.size`, `.named_count`).
#' @param ... Additional arguments passed to `FUN`.
#' @return A new `flexseq` with transformed elements.
#' @export
fapply.flexseq <- function(X, FUN, ..., preserve_custom_monoids = TRUE) {
  if(!is.function(FUN)) {
    stop("`FUN` must be a function.")
  }
  .seq_apply_impl(X, FUN, ..., preserve_custom_monoids = preserve_custom_monoids)
}
