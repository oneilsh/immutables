# Runtime: O(n log n) total (O(n) traversal + O(n log n) rebuild).
.seq_apply_impl <- function(x, f, ...) {
  if(!inherits(x, "flexseq")) {
    stop("`x` must be a flexseq.")
  }
  if(!is.function(f)) {
    stop("`f` must be a function.")
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

  out_monoids <- resolve_tree_monoids(x, required = TRUE)

  as_flexseq(out, monoids = out_monoids)
}

#' Apply a function over flexseq elements
#'
#' @method fapply flexseq
#' @param X A `flexseq`.
#' @param FUN Function to apply to each element.
#' @param ... Additional arguments passed to `FUN`.
#' @return A new `flexseq` with transformed elements.
#' @export
fapply.flexseq <- function(X, FUN, ...) {
  if(!is.function(FUN)) {
    stop("`FUN` must be a function.")
  }
  .seq_apply_impl(X, FUN, ...)
}
