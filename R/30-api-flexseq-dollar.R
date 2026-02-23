# parse `$` name argument into a scalar character key.
# Runtime: O(1).
.ft_dollar_name(name_expr) %::% . : character
.ft_dollar_name(name_expr) %as% {
  nm <- as.character(name_expr)
  if(length(nm) != 1L || is.na(nm) || nm == "") {
    stop("`$` expects a single valid name.")
  }
  nm
}

#' @rdname sub-.flexseq
#' @method $ flexseq
#' @param name Element name (for `$` and `$<-`).
#' @return For `$`: the matched element.
#' @examples
#'
#' # $ extracts by name
#' x <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
#' x$b
#' @export
# Runtime: O(1) for structural-field fallback on unnamed trees; O(n) worst-case
# for name lookup (single-name locate with early exit).
`$.flexseq` <- function(x, name) {
  nm <- .ft_dollar_name(substitute(name))
  # Preserve structural-field `$` access on unnamed trees for internal traversal
  # and developer ergonomics. Use actual list-field names instead of hard-coding.
  ms <- attr(x, "measures", exact = TRUE)
  nn <- if(!is.null(ms) && !is.null(ms[[".named_count"]])) as.integer(ms[[".named_count"]]) else NA_integer_
  if(!is.na(nn) && nn == 0L) {
    if(nm %in% names(unclass(x))) {
      return(.subset2(x, nm))
    }
  }
  `[[.flexseq`(x, nm)
}

#' @export
#' @noRd
`$.ordered_sequence` <- function(x, name) {
  nm <- .ft_dollar_name(substitute(name))
  `[[.ordered_sequence`(x, nm)
}

#' @export
#' @noRd
`$.priority_queue` <- function(x, name) {
  nm <- .ft_dollar_name(substitute(name))
  `[[.priority_queue`(x, nm)
}

#' @rdname sub-.interval_index
#' @method $ interval_index
#' @param name Element name (for `$` and `$<-`).
#' @return For `$`: the matched payload element.
#' @export
`$.interval_index` <- function(x, name) {
  nm <- .ft_dollar_name(substitute(name))
  `[[.interval_index`(x, nm)
}

#' @rdname sub-.flexseq
#' @method $<- flexseq
#' @return For `$<-`: updated tree with one named element replaced.
#' @examples
#'
#' # $<- replaces by name
#' x <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
#' x$b <- 20
#' x$b
#' @export
# Runtime: O(n), delegated to `[[<-.flexseq`.
`$<-.flexseq` <- function(x, name, value) {
  nm <- .ft_dollar_name(substitute(name))
  `[[<-.flexseq`(x, nm, value)
}

#' @export
#' @noRd
`$<-.ordered_sequence` <- function(x, name, value) {
  .ft_stop_ordered_like(x, "$<-", "Replacement indexing is not supported.")
}

#' @export
#' @noRd
`$<-.priority_queue` <- function(x, name, value) {
  stop("`$<-` is not supported for priority_queue. Cast first with `as_flexseq()`.")
}

#' @rdname sub-.interval_index
#' @method $<- interval_index
#' @param value Replacement value (unsupported).
#' @return No return value; always errors because replacement indexing is unsupported.
#' @export
`$<-.interval_index` <- function(x, name, value) {
  stop("`$<-` is not supported for interval_index.")
}
