#' Split a flexseq into Left and Right Parts by Predicate
#'
#' Splits a sequence at the point where the predicate first becomes TRUE on
#' accumulated monoid measures.
#'
#' @param x A `flexseq`.
#' @param predicate Predicate function on accumulated measure values.
#' @param monoid_name Character scalar naming the monoid to scan.
#' @return A list with `left` and `right` flexseq objects.
#' @examples
#' x <- as_flexseq(letters[1:6])
#' x
#'
#' s <- split_by_predicate(x, function(v) v >= 4, ".size")
#' s$left
#' s$right
#' @export
# Runtime: O(log n) near split point depth.
split_by_predicate <- function(x, predicate, monoid_name) {
  if(inherits(x, "priority_queue")) {
    stop("`split_by_predicate()` is not supported for priority_queue. Cast first with `as_flexseq()`.")
  }
  ctx <- resolve_named_monoid(x, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid

  if(x %isa% Empty) {
    return(list(left = .as_flexseq(measured_empty(ms)), right = .as_flexseq(measured_empty(ms))))
  }

  if(predicate(node_measure(x, monoid_name))) {
    s <- if(.ft_cpp_can_use(ms)) {
      .ft_cpp_split_tree(x, predicate, ms, monoid_name, mr$i)
    } else {
      split_tree_impl_fast(predicate, mr$i, x, ms, mr, monoid_name)
    }
    right <- push_front(s$right, s$elem)
    return(list(left = .as_flexseq(s$left), right = .as_flexseq(right)))
  }

  list(left = .as_flexseq(x), right = .as_flexseq(measured_empty(ms)))
}

#' Split by Scalar Index or Name
#'
#' Splits by element position (`.size` measure) after resolving `at` to a single
#' index. `at` can be a positive scalar integer index or a scalar character name.
#'
#' @param x A `flexseq`.
#' @param at Positive scalar integer index or scalar character name.
#' @param pull_index Logical switch between two-way and three-way split shape.
#'   If `TRUE`, uses [split_by_predicate()] and returns `list(left, right)`.
#'   If `FALSE`, uses [split_around_by_predicate()] and returns
#'   `list(left, elem, right)`.
#' @return A split list, shape controlled by `pull_index`.
#' @examples
#' x <- as_flexseq(setNames(as.list(letters[1:6]), LETTERS[1:6]))
#' split_at(x, 3)
#' split_at(x, "C", pull_index = TRUE)
#' @export
# Runtime: O(log n) for scalar index splits; O(n) for scalar name lookup + split.
split_at <- function(x, at, pull_index = FALSE) {
  if(inherits(x, "priority_queue")) {
    stop("`split_at()` is not supported for priority_queue. Cast first with `as_flexseq()`.")
  }
  if(is.logical(at)) {
    stop("`at` must be a scalar index or name.")
  }

  idx <- NULL
  if(is.numeric(at) && length(at) == 1L && !is.na(at) && at == as.integer(at)) {
    n <- as.integer(node_measure(x, ".size"))
    idx <- .ft_assert_int_indices(as.integer(at), n)
    if(length(idx) != 1L) {
      stop("`at` must be a single valid index.")
    }
    idx <- as.integer(idx[[1L]])
  } else if(is.character(at) && length(at) == 1L && !is.na(at) && at != "") {
    idx <- .ft_match_name_indices(x, at, strict_missing = TRUE)
    idx <- as.integer(idx[[1L]])
  } else {
    stop("`at` must be a positive scalar integer index or a scalar non-empty name.")
  }

  predicate <- function(v) v >= idx
  if(isTRUE(pull_index)) {
    return(split_by_predicate(x, predicate, ".size"))
  }
  split_around_by_predicate(x, predicate, ".size")
}
