#' Locate First Predicate Flip Without Reconstructing Context Trees
#'
#' Read-only analogue of [split_around_by_predicate()]: finds the distinguished
#' element where the scan predicate flips, but does not rebuild left/right
#' trees.
#'
#' @param t A `flexseq`.
#' @param predicate Function on accumulated measure values.
#' @param monoid_name Name of monoid from `attr(t, "monoids")`.
#' @param accumulator Optional starting measure (defaults to monoid identity).
#' @param include_metadata Logical; include left/hit/right measures and index.
#' @return If `include_metadata = FALSE`: `list(found, elem)`.
#'   If `TRUE`: `list(found, elem, metadata = list(left_measure, hit_measure,
#'   right_measure, index))`.
#' @examples
#' x <- as_flexseq(letters[1:6])
#' x
#'
#' loc <- locate_by_predicate(x, function(v) v >= 4, ".size")
#' loc
#' 
#' # include metadata with a custom monoid
#' sum_m <- measure_monoid(`+`, 0, as.numeric)
#' x2 <- add_monoids(as_flexseq(1:6), list(sum = sum_m))
#' loc2 <- locate_by_predicate(x2, function(v) v >= 10, "sum", include_metadata = TRUE)
#' loc2
#' @export
# Runtime: O(log n) near locate point depth.
locate_by_predicate <- function(t, predicate, monoid_name, accumulator = NULL, include_metadata = FALSE) {
  if(inherits(t, "priority_queue")) {
    stop("`locate_by_predicate()` is not supported for priority_queue. Cast first with `as_flexseq()`.")
  }
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

  res <- if(.ft_cpp_can_use(ms)) {
    .ft_cpp_locate(t, predicate, ms, monoid_name, i)
  } else {
    locate_tree_impl_fast(predicate, i, t, ms, mr, monoid_name, 0L)
  }
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
