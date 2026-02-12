#' Locate first predicate flip without reconstructing context trees
#'
#' Read-only analogue of `split_tree()`: finds the distinguished element where
#' the scan predicate flips, but does not rebuild left/right trees.
#'
#' @param t FingerTree.
#' @param predicate Function on accumulated measure values.
#' @param monoid_name Name of monoid from `attr(t, "monoids")`.
#' @param accumulator Optional starting measure (defaults to monoid identity).
#' @param include_metadata Logical; include left/hit/right measures and index.
#' @return If `include_metadata = FALSE`: `list(found, elem)`.
#'   If `TRUE`: `list(found, elem, metadata = list(left_measure, hit_measure,
#'   right_measure, index))`.
#' @examples
#' t <- tree_from(letters[1:6])
#' locate(t, function(v) v >= 4, ".size")
#'
#' # Metadata-rich locate for custom monoid
#' sum_m <- MeasureMonoid(`+`, 0, as.numeric)
#' t2 <- tree_from(1:6, monoids = list(sum = sum_m))
#' locate(t2, function(v) v >= 10, "sum", include_metadata = TRUE)
#' @export
# Runtime: O(log n) near locate point depth.
locate <- function(t, predicate, monoid_name, accumulator = NULL, include_metadata = FALSE) {
  ctx <- resolve_named_monoid(t, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid

  if(t %isa% Empty) {
    if(!isTRUE(include_metadata)) {
      return(list(found = FALSE, elem = NULL))
    }
    i0 <- if(is.null(accumulator)) mr$i else accumulator
    return(list(
      found = FALSE,
      elem = NULL,
      metadata = list(left_measure = i0, hit_measure = NULL, right_measure = NULL, index = NULL)
    ))
  }

  i <- if(is.null(accumulator)) mr$i else accumulator
  total <- mr$f(i, node_measure(t, monoid_name))

  if(!predicate(total)) {
    if(!isTRUE(include_metadata)) {
      return(list(found = FALSE, elem = NULL))
    }
    return(list(
      found = FALSE,
      elem = NULL,
      metadata = list(left_measure = total, hit_measure = NULL, right_measure = NULL, index = NULL)
    ))
  }

  res <- locate_tree_impl(predicate, i, t, ms, monoid_name, 0L)
  if(!isTRUE(include_metadata)) {
    return(list(found = res$found, elem = res$elem))
  }

  list(
    found = res$found,
    elem = res$elem,
    metadata = list(
      left_measure = res$left_measure,
      hit_measure = res$hit_measure,
      right_measure = res$right_measure,
      index = res$index
    )
  )
}
