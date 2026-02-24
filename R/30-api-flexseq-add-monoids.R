#' Add/merge monoids on an existing tree
#'
#' @param t FingerTree.
#' @param monoids Named list of `measure_monoid` objects to add.
#' @param overwrite Logical; whether overlapping names replace existing monoids.
#' @return A persistent copy with recomputed cached measures.
#'
#' @details
#' `add_monoids()` is the advanced user-facing API for attaching custom monoids
#' after constructing a structure.
#'
#' The `measure(el)` input shape depends on structure type:
#'
#' - `flexseq`: payload element.
#' - `priority_queue`: entry list with `item` and `priority`.
#' - `ordered_sequence`: entry list with `item` and `key`.
#' - `interval_index`: entry list with `item`, `start`, and `end`.
#'
#' @examples
#' # Common workflow:
#' # 1) construct structure
#' # 2) attach monoid with add_monoids()
#' # 3) query with split_around_by_predicate()
#'
#' # flexseq: split a stream by cumulative token budget
#' tokens <- as_flexseq(c(120, 80, 50, 200))
#' token_sum <- measure_monoid(`+`, 0, as.numeric)
#' budgeted <- add_monoids(tokens, list(token_sum = token_sum))
#' cut <- split_around_by_predicate(budgeted, function(v) v > 200, "token_sum")
#' cut$left   # still within budget
#' cut$elem   # first element that crosses budget
#' cut$right  # remaining tail
#'
#' # interval_index: split by cumulative interval width
#' ix <- interval_index("A", "B", "C", start = c(0, 4, 9), end = c(3, 8, 10))
#' width_sum <- measure_monoid(`+`, 0, function(el) as.numeric(el$end - el$start))
#' ix2 <- add_monoids(ix, list(width_sum = width_sum))
#' cut_ix <- split_around_by_predicate(as_flexseq(ix2), function(v) v > 6, "width_sum")
#' cut_ix$left        # intervals before width budget is exceeded
#' cut_ix$elem        # first interval that crosses width budget (value at cut_ix$elem$item)
#' cut_ix$right       # remaining interval
#' @export
# Runtime: O(n) over tree size for any non-trivial update (rebind/recompute pass).
add_monoids <- function(t, monoids, overwrite = FALSE) {
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
