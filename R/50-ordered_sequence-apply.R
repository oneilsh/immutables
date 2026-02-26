#SO

# Runtime: O(n) from traversal + ordered bulk rebuild.
.oms_apply_impl <- function(x, f, ..., preserve_custom_monoids = TRUE) {
  .oms_assert_set(x)
  if(!is.function(f)) {
    stop("`FUN` must be a function.")
  }
  if(!is.logical(preserve_custom_monoids) || length(preserve_custom_monoids) != 1L || is.na(preserve_custom_monoids)) {
    stop("`preserve_custom_monoids` must be TRUE or FALSE.")
  }

  entries <- as.list.flexseq(x)
  n <- length(entries)
  if(n == 0L) {
    return(x)
  }

  out <- vector("list", n)
  out_names <- if(is.null(names(entries))) rep("", n) else names(entries)

  for(i in seq_len(n)) {
    e <- entries[[i]]
    cur_name <- out_names[[i]]

    item2 <- f(e$item, e$key, cur_name, ...)
    out[[i]] <- .oms_make_entry(item = item2, key_value = e$key)
  }

  if(any(out_names != "")) {
    names(out) <- out_names
  }

  ms <- if(isTRUE(preserve_custom_monoids)) {
    attr(x, "monoids", exact = TRUE)
  } else {
    .oms_merge_monoids(NULL)
  }
  out_tree <- .oms_tree_from_ordered_entries(out, ms)
  .ord_wrap_like(x, out_tree, key_type = .oms_key_type_state(x))
}

#' Apply a function over ordered sequence entries
#'
#' @method fapply ordered_sequence
#' @param X An `ordered_sequence`.
#' @param FUN Function of `(item, key, name, ...)` returning the new payload
#'   item. Key metadata (`key`, `name`) is read-only.
#' @param preserve_custom_monoids Logical scalar. If `TRUE` (default), preserve
#'   attached user monoids during rebuild. If `FALSE`, drop user monoids and
#'   keep only required ordered/structural monoids.
#' @param ... Additional arguments passed to `FUN`.
#' @return A new `ordered_sequence` with transformed entries.
#' @export
fapply.ordered_sequence <- function(X, FUN, ..., preserve_custom_monoids = TRUE) {
  if(!is.function(FUN)) {
    stop("`FUN` must be a function.")
  }
  .oms_apply_impl(X, FUN, ..., preserve_custom_monoids = preserve_custom_monoids)
}
