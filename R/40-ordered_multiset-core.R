# Runtime: O(1).
.oms_assert_multiset <- function(x) {
  if(!inherits(x, "ordered_multiset") || !is_structural_node(x)) {
    stop("`x` must be an ordered_multiset.")
  }
  invisible(TRUE)
}

# Runtime: O(n log n) from build and ordering.
#' Build an Ordered Multiset from elements
#'
#' @param x Elements to add.
#' @param keys Scalar key values matching `x` length.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#' @return An `ordered_multiset`.
#' @examples
#' ms <- as_ordered_multiset(c(4, 1, 2, 1), keys = c(4, 1, 2, 1))
#' ms
#' union(ms, ms)
#' @export
as_ordered_multiset <- function(x, keys = NULL, monoids = NULL) {
  .oms_build_from_items(as.list(x), keys = keys, monoids = monoids, concrete_class = "ordered_multiset")
}

# Runtime: O(n log n) from build and ordering.
#' Construct an Ordered Multiset
#'
#' @param ... Elements to add.
#' @param keys Scalar key values matching `...` length.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#' @return An `ordered_multiset`.
#' @examples
#' ms <- ordered_multiset("bb", "a", "ccc", keys = c(2, 1, 3))
#' ms
#' union(ms, ms)
#' @export
ordered_multiset <- function(..., keys = NULL, monoids = NULL) {
  as_ordered_multiset(list(...), keys = keys, monoids = monoids)
}

# Runtime: O(n + m) key merge scan + O(n + m) bulk build.
#' Multiset union (bag semantics)
#'
#' @rdname union
#' @method union ordered_multiset
#' @return `ordered_multiset` where multiplicity is `max(count_x, count_y)`.
#' @export
union.ordered_multiset <- function(x, y, ...) {
  .oms_assert_multiset(x)
  .oms_assert_multiset(y)
  .oms_set_merge(x, y, mode = "union")
}

# Runtime: O(n + m) key merge scan + O(n + m) bulk build.
#' Multiset intersection (bag semantics)
#'
#' @rdname union
#' @method intersect ordered_multiset
#' @return `ordered_multiset` where multiplicity is `min(count_x, count_y)`.
#' @export
intersect.ordered_multiset <- function(x, y, ...) {
  .oms_assert_multiset(x)
  .oms_assert_multiset(y)
  .oms_set_merge(x, y, mode = "intersection")
}

# Runtime: O(n + m) key merge scan + O(n + m) bulk build.
#' Multiset difference (bag semantics)
#'
#' @rdname union
#' @method setdiff ordered_multiset
#' @return `ordered_multiset` where multiplicity is `pmax(count_x - count_y, 0)`.
#' @export
setdiff.ordered_multiset <- function(x, y, ...) {
  .oms_assert_multiset(x)
  .oms_assert_multiset(y)
  .oms_set_merge(x, y, mode = "difference")
}
