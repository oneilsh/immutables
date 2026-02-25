#SO

# Runtime: O(n) over tree size for any non-trivial update (rebind/recompute pass).
#' @method add_monoids flexseq
#' @export
add_monoids.flexseq <- function(t, monoids, overwrite = FALSE) {
  if(length(monoids) > 0L) {
    bad <- intersect(names(monoids), c(".size", ".named_count"))
    if(length(bad) > 0L) {
      stop("Reserved monoid names cannot be supplied for flexseq: ", paste(bad, collapse = ", "))
    }
  }
  add <- ensure_size_monoids(monoids)
  cur <- resolve_tree_monoids(t, required = TRUE)
  merged <- merge_monoid_sets(cur, add, overwrite = overwrite)

  add_only <- setdiff(names(add), names(cur))
  overlap <- setdiff(intersect(names(add), names(cur)), c(".size", ".named_count"))
  recompute_names <- add_only
  if(isTRUE(overwrite) && length(overlap) > 0) {
    recompute_names <- union(recompute_names, overlap)
  }
  recompute_names <- setdiff(recompute_names, c(".size", ".named_count"))

  if(length(recompute_names) == 0) {
    return(t)
  }

  out <- rebind_tree_monoids(t, merged, recompute_names)
  .ft_restore_subclass(out, t, context = "add_monoids()")
}
