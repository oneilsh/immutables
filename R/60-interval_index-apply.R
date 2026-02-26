.ivx_apply_impl <- function(x, f, ...) {
  .ivx_assert_index(x)
  if(!is.function(f)) {
    stop("`FUN` must be a function.")
  }

  entries <- .ivx_entries(x)
  n <- length(entries)
  if(n == 0L) {
    return(x)
  }

  out_entries <- vector("list", n)

  for(i in seq_len(n)) {
    e <- entries[[i]]
    nm <- attr(e, "ft_name", exact = TRUE)
    if(
      is.null(nm) ||
      length(nm) == 0L ||
      !is.character(nm) ||
      length(nm) != 1L ||
      is.na(nm) ||
      !nzchar(nm)
    ) {
      nm <- .ft_get_name(e)
    }
    cur_name <- if(is.null(nm)) "" else nm

    item2 <- f(e$item, e$start, e$end, cur_name, ...)
    entry2 <- .ivx_make_entry(item2, e$start, e$end)
    out_entries[[i]] <- .ft_set_name(entry2, nm)
  }

  ms <- resolve_tree_monoids(x, required = TRUE)
  out_tree <- .ivx_tree_from_ordered_entries(out_entries, ms)
  .ivx_wrap_like(x, out_tree)
}

# Runtime: O(n) from traversal + ordered bulk rebuild.
#' Apply a function over interval index entries
#'
#' @method fapply interval_index
#' @param X An `interval_index`.
#' @param FUN Function of `(item, start, end, name, ...)` returning the new
#'   payload item. Interval metadata (`start`, `end`, `name`) is read-only.
#' @param ... Additional arguments passed to `FUN`.
#' @return A new `interval_index` with transformed entries.
#' @export
fapply.interval_index <- function(X, FUN, ...) {
  if(!is.function(FUN)) {
    stop("`FUN` must be a function.")
  }
  .ivx_apply_impl(X, FUN, ...)
}
