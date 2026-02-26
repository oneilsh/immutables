# Runtime: O(n) from list materialization + linear rebuild.
.as_flexseq_build.interval_index <- function(x, monoids = NULL) {
  entries <- as.list.flexseq(x)
  out_monoids <- monoids
  if(is.null(out_monoids)) {
    ms <- attr(x, "monoids", exact = TRUE)
    out_monoids <- ms[setdiff(names(ms), c(".ivx_max_start", ".ivx_max_end", ".ivx_min_end", ".oms_max_key"))]
  }
  .as_flexseq_build.default(entries, monoids = out_monoids)
}

#' @method as_flexseq interval_index
#' @export
# Runtime: O(n) from list materialization + linear rebuild.
as_flexseq.interval_index <- function(x) {
  .as_flexseq_build.interval_index(x, monoids = NULL)
}

# Runtime: O(n).
#' Coerce Interval Index to List
#'
#' @method as.list interval_index
#' @param x An `interval_index`.
#' @param ... Unused.
#' @return A plain list of payload elements in interval order.
#' @export
as.list.interval_index <- function(x, ...) {
  .ivx_assert_index(x)
  .ivx_extract_items(as.list.flexseq(x, ...))
}

# Runtime: O(1).
#' Interval Index Length
#'
#' @method length interval_index
#' @param x An `interval_index`.
#' @return Integer length.
#' @export
length.interval_index <- function(x) {
  as.integer(node_measure(x, ".size"))
}

#' Plot an Interval Index Tree
#'
#' @method plot interval_index
#' @param x An `interval_index`.
#' @param ... Passed to the internal tree plotting routine.
#' @export
# Runtime: O(n) to build plot graph data.
plot.interval_index <- function(x, ...) {
  plot.flexseq(x, ...)
}
