# parse `$` name argument into a scalar character key.
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
#' @method $ FingerTree
#' @param x FingerTree.
#' @param name Element name.
#' @return The matched element.
#' @examples
#' t <- tree_from(setNames(as.list(1:3), c("a", "b", "c")))
#' t$b
#' @export
`$.FingerTree` <- function(x, name) {
  nm <- .ft_dollar_name(substitute(name))
  # Preserve structural-field `$` access on unnamed trees for internal traversal
  # and developer ergonomics. Use actual list-field names instead of hard-coding.
  if(nm %in% names(unclass(x))) {
    nn <- as.integer(node_measure(x, ".named_count"))
    if(nn == 0L) {
      return(.subset2(x, nm))
    }
  }
  `[[.FingerTree`(x, nm)
}

#' Replace a named element with `$<-`
#'
#' Exact-name replacement on named trees, equivalent to `x[[\"name\"]] <- value`.
#'
#' @method $<- FingerTree
#' @param x FingerTree.
#' @param name Element name.
#' @param value Replacement element.
#' @return Updated tree.
#' @examples
#' t <- tree_from(setNames(as.list(1:3), c("a", "b", "c")))
#' t$b <- 20
#' t$b
#' @export
`$<-.FingerTree` <- function(x, name, value) {
  nm <- .ft_dollar_name(substitute(name))
  `[[<-.FingerTree`(x, nm, value)
}
