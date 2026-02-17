# Runtime: O(n log n) total (O(n) traversal + O(n log n) rebuild).
.seq_apply_impl <- function(x, f, preserve_monoids = FALSE, ...) {
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

#' Apply over flexseq elements
#'
#' @rdname apply
#' @method apply flexseq
#' @param preserve_monoids Logical; when `TRUE`, carries all input monoids to
#'   the output. When `FALSE` (default), output keeps only invariant monoids.
#' @export
apply.flexseq <- function(X, MARGIN = NULL, FUN = NULL, ..., preserve_monoids = FALSE) {
  if(is.null(FUN)) {
    if(is.function(MARGIN)) {
      FUN <- MARGIN
      MARGIN <- NULL
    } else {
      stop("`FUN` must be a function.")
    }
  }
  if(!is.null(MARGIN)) {
    stop("`MARGIN` is not used for flexseq; call `apply(x, FUN, ...)`.")
  }
  .seq_apply_impl(X, FUN, preserve_monoids = preserve_monoids, ...)
}
