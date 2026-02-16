#' Set operations with S3 dispatch
#'
#' `union()`, `intersect()`, and `setdiff()` are exported as generics so custom
#' structures (such as `ordered_multiset`) can provide method implementations
#' while preserving base behavior for standard vectors.
#'
#' @param x First object.
#' @param y Second object.
#' @param ... Method-specific arguments.
#' @return Method-dependent result.
#' @export
union <- function(x, y, ...) {
  UseMethod("union")
}

#' @export
#' @noRd
union.default <- function(x, y, ...) {
  base::union(x, y, ...)
}

#' @rdname union
#' @export
intersect <- function(x, y, ...) {
  UseMethod("intersect")
}

#' @export
#' @noRd
intersect.default <- function(x, y, ...) {
  base::intersect(x, y, ...)
}

#' @rdname union
#' @export
setdiff <- function(x, y, ...) {
  UseMethod("setdiff")
}

#' @export
#' @noRd
setdiff.default <- function(x, y, ...) {
  base::setdiff(x, y, ...)
}
