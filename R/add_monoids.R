#' Add/merge monoids on an existing tree
#'
#' @param t FingerTree.
#' @param monoids Named list of `MeasureMonoid` objects to add.
#' @param overwrite Logical; whether overlapping names replace existing monoids.
#' @return A persistent copy with recomputed cached measures.
#' @examples
#' t <- tree_from(1:5)
#' sum_m <- MeasureMonoid(`+`, 0, as.numeric)
#' t2 <- add_monoids(t, list(sum = sum_m))
#' attr(t2, "measures")$sum
#'
#' # Replace an existing monoid definition
#' sum2 <- MeasureMonoid(function(a, b) a + 2 * b, 0, as.numeric)
#' t3 <- add_monoids(t2, list(sum = sum2), overwrite = TRUE)
#' attr(t3, "measures")$sum
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

  rebind_tree_monoids(t, merged, recompute_names)
}
