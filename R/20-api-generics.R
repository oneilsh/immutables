#SO

#' Fapply with S3 dispatch
#'
#' `fapply()` is an S3 generic for applying functions over immutable
#' structures with type-specific dispatch.
#'
#' @param X Object to apply over.
#' @param FUN Function to apply.
#' @param ... Method-specific arguments.
#' @return Method-dependent result.
#' @seealso [fapply.flexseq()], [fapply.priority_queue()], [fapply.ordered_sequence()], [fapply.interval_index()]
#' @export
fapply <- function(X, FUN, ...) {
  UseMethod("fapply")
}

#' Add/merge monoids on an existing tree
#'
#' @param t FingerTree.
#' @param monoids Named list of `measure_monoid` objects to add.
#' @param overwrite Logical; whether overlapping names replace existing monoids.
#' @return A persistent copy with recomputed cached measures.
#' @export
#' @seealso [add_monoids.flexseq()], [add_monoids.priority_queue()],
#'   [add_monoids.ordered_sequence()], [add_monoids.interval_index()]
# Runtime: O(1) dispatch.
add_monoids <- function(t, monoids, overwrite = FALSE) {
  UseMethod("add_monoids")
}

# Runtime: O(1).
#' @export
#' @noRd
add_monoids.default <- function(t, monoids, overwrite = FALSE) {
  cls <- class(t)
  cls_txt <- if(length(cls) == 0L) "unknown" else paste(cls, collapse = "/")
  stop(sprintf("No `add_monoids()` method for class '%s'.", cls_txt))
}

#' Coerce to flexseq
#'
#' @param x Input vector/list-like object.
#' @return A `flexseq` object.
#' @export
# Runtime: O(1) generic dispatch.
as_flexseq <- function(x) {
  UseMethod("as_flexseq")
}

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
#' @details
#' For `priority_queue` objects, `metadata$index` (when requested) is the
#' internal structural position in the underlying sequence representation. It is
#' not related to priority rank and is not stable across queue updates, so it
#' should be treated as diagnostic metadata only.
#' @return If `include_metadata = FALSE`: `list(found, elem)`.
#'   If `TRUE`: `list(found, elem, metadata = list(left_measure, hit_measure,
#'   right_measure, index))`.
#' @export
locate_by_predicate <- function(t, predicate, monoid_name, accumulator = NULL, include_metadata = FALSE) {
  UseMethod("locate_by_predicate")
}

#' Split Around First Predicate Flip
#'
#' Splits a sequence into left context, matched element, and right context at
#' the first point where `predicate` becomes `TRUE` on accumulated monoid
#' measures.
#'
#' @param t A `flexseq`.
#' @param predicate Function on accumulated measure values.
#' @param monoid_name Name of monoid from `attr(t, "monoids")`.
#' @param accumulator Optional starting measure (defaults to monoid identity).
#' @return A list with `left`, `elem`, and `right`.
#' @export
split_around_by_predicate <- function(t, predicate, monoid_name, accumulator = NULL) {
  UseMethod("split_around_by_predicate")
}

#' Split a flexseq into Left and Right Parts by Predicate
#'
#' Splits a sequence at the point where the predicate first becomes TRUE on
#' accumulated monoid measures.
#'
#' @param x A `flexseq`.
#' @param predicate Predicate function on accumulated measure values.
#' @param monoid_name Character scalar naming the monoid to scan.
#' @return A list with `left` and `right` flexseq objects.
#' @export
split_by_predicate <- function(x, predicate, monoid_name) {
  UseMethod("split_by_predicate")
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
#' @export
split_at <- function(x, at, pull_index = FALSE) {
  UseMethod("split_at")
}

#' Insert an element
#'
#' Generic `insert()` dispatches by class.
#'
#' @param x Object to insert into.
#' @param ... Method-specific arguments.
#' @return Updated object.
#' @seealso [insert.priority_queue()], [insert.ordered_sequence()], [insert.interval_index()]
#' @export
insert <- function(x, ...) {
  UseMethod("insert")
}

#' @export
#' @noRd
insert.default <- function(x, ...) {
  cls <- class(x)
  cls_txt <- if(length(cls) == 0L) "unknown" else paste(cls, collapse = "/")
  stop(sprintf("No `insert()` method for class '%s'.", cls_txt))
}
