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

#' Extract a named element with `$`
#'
#' Exact-name lookup on named trees, equivalent to `x[[\"name\"]]`.
#'
#' @method $ flexseq
#' @param x A `flexseq`.
#' @param name Element name.
#' @return The matched element.
#' @examples
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

#' Replace a named element with `$<-`
#'
#' Exact-name replacement on named trees, equivalent to `x[[\"name\"]] <- value`.
#'
#' @method $<- flexseq
#' @param x A `flexseq`.
#' @param name Element name.
#' @param value Replacement element.
#' @return Updated tree.
#' @examples
#' x <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
#' x$b <- 20
#' x$b
#' @export
# Runtime: O(n), delegated to `[[<-.flexseq`.
`$<-.flexseq` <- function(x, name, value) {
  nm <- .ft_dollar_name(substitute(name))
  `[[<-.flexseq`(x, nm, value)
}
