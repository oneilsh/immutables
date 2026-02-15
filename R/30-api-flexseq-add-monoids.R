#' Add/merge monoids on an existing tree
#'
#' @param t FingerTree.
#' @param monoids Named list of `measure_monoid` objects to add.
#' @param overwrite Logical; whether overlapping names replace existing monoids.
#' @return A persistent copy with recomputed cached measures.
#' @examples
#' x <- as_flexseq(1:5)
#' x
#'
#' sum_m <- measure_monoid(`+`, 0, as.numeric)
#' x2 <- add_monoids(x, list(sum = sum_m))
#' x2
#' attr(x2, "measures")$sum
#'
#' # replace an existing monoid definition
#' prod_m <- measure_monoid(`*`, 1, as.numeric)
#' x3 <- add_monoids(x2, list(sum = prod_m), overwrite = TRUE)
#' attr(x3, "measures")$sum
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
