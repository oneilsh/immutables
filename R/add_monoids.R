#' Add/merge monoids on an existing tree
#'
#' @param t FingerTree.
#' @param monoids Named list of `MeasureMonoid` objects to add.
#' @param overwrite Logical; whether overlapping names replace existing monoids.
#' @return A persistent copy with recomputed cached measures.
#' @export
add_monoids <- function(t, monoids, overwrite = FALSE) {
  assert_structural_attrs(t)
  add <- ensure_size_monoids(monoids)
  cur <- resolve_tree_monoids(t, required = TRUE)
  merged <- merge_monoid_sets(cur, add, overwrite = overwrite)

  add_only <- setdiff(names(add), names(cur))
  overlap <- setdiff(intersect(names(add), names(cur)), ".size")
  recompute_names <- add_only
  if(isTRUE(overwrite) && length(overlap) > 0) {
    recompute_names <- union(recompute_names, overlap)
  }
  recompute_names <- setdiff(recompute_names, ".size")

  if(length(recompute_names) == 0) {
    return(t)
  }

  t2 <- rebind_tree_monoids(t, merged, recompute_names)
  assert_structural_attrs(t2)
  t2
}
