# Runtime: O(n) from traversal + ordered bulk rebuild.
.oms_apply_impl <- function(x, f, ...) {
  .oms_assert_set(x)
  if(!is.function(f)) {
    stop("`FUN` must be a function.")
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

  ms <- attr(x, "monoids", exact = TRUE)
  out_tree <- .oms_tree_from_ordered_entries(out, ms)
  .ord_wrap_like(x, out_tree, key_type = .oms_key_type_state(x))
}

#' Apply a function over ordered sequence entries
#'
#' @method fapply ordered_sequence
#' @param X An `ordered_sequence`.
#' @param FUN Function of `(item, key, name, ...)` returning the new payload
#'   item. Key metadata (`key`, `name`) is read-only.
#' @param ... Additional arguments passed to `FUN`.
#' @return A new `ordered_sequence` with transformed entries.
#' @export
fapply.ordered_sequence <- function(X, FUN, ...) {
  if(!is.function(FUN)) {
    stop("`FUN` must be a function.")
  }
  .oms_apply_impl(X, FUN, ...)
}
