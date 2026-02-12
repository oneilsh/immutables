#' Validate full tree invariants (debug/test utility)
#'
#' Performs expensive full-tree auditing of:
#' - structural attributes (`monoids`/`measures`) consistency
#' - global name-state invariants
#'
#' Intended for debugging and tests, not hot runtime paths.
#'
#' @param t FingerTree.
#' @return `TRUE` invisibly; errors if invariant violations are found.
#' @examples
#' t <- tree_from(letters[1:10])
#' validate_tree(t)
#' @export
# Runtime: O(n) full traversal. Intended for debugging/tests.
validate_tree <- function(t) {
  assert_structural_attrs(t)
  .ft_assert_name_state(t)
  invisible(TRUE)
}

#' Validate name-state invariants only (debug/test utility)
#'
#' Checks that trees are either fully unnamed or fully named with unique,
#' non-empty names.
#'
#' Intended for debugging and tests, not hot runtime paths.
#'
#' @param t FingerTree.
#' @return `TRUE` invisibly; errors if name invariants are violated.
#' @examples
#' t <- tree_from(setNames(as.list(letters[1:4]), letters[1:4]))
#' validate_name_state(t)
#' @export
# Runtime: O(n) when tree is named (name collection and uniqueness checks).
validate_name_state <- function(t) {
  .ft_assert_name_state(t)
  invisible(TRUE)
}
