#' Split a flexseq into left and right parts
#'
#' Splits a sequence at the point where the predicate first becomes TRUE on
#' accumulated monoid measures.
#'
#' @method split flexseq
#' @param x A `flexseq`.
#' @param f Predicate function on accumulated measure values.
#' @param ... Additional arguments. The first positional argument is
#'   `monoid_name` (character scalar naming the monoid to scan).
#' @return A list with `left` and `right` flexseq objects.
#' @examples
#' x <- as_flexseq(letters[1:6])
#' x
#'
#' s <- split(x, function(v) v >= 4, ".size")
#' s$left
#' s$right
#' @export
# Runtime: O(log n) near split point depth.
split.flexseq <- function(x, f, ...) {
  dots <- list(...)
  if(length(dots) < 1L || !is.character(dots[[1L]])) {
    stop("`monoid_name` (character) is required as the third argument to split().")
  }
  monoid_name <- dots[[1L]]

  ctx <- resolve_named_monoid(x, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid

  if(x %isa% Empty) {
    return(list(left = .as_flexseq(measured_empty(ms)), right = .as_flexseq(measured_empty(ms))))
  }

  if(f(node_measure(x, monoid_name))) {
    s <- if(.ft_cpp_can_use(ms)) {
      .ft_cpp_split_tree(x, f, ms, monoid_name, mr$i)
    } else {
      split_tree_impl_fast(f, mr$i, x, ms, mr, monoid_name)
    }
    right <- prepend(s$right, s$elem)
    return(list(left = .as_flexseq(s$left), right = .as_flexseq(right)))
  }

  list(left = .as_flexseq(x), right = .as_flexseq(measured_empty(ms)))
}
