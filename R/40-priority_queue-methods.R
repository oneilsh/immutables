#SO

# Runtime: O(n) from list materialization + linear rebuild.
.as_flexseq_build.priority_queue <- function(x, monoids = NULL) {
  entries <- as.list(x)
  out_monoids <- monoids
  if(is.null(out_monoids)) {
    ms <- attr(x, "monoids", exact = TRUE)
    out_monoids <- ms[setdiff(names(ms), c(".pq_min", ".pq_max"))]
  }
  .as_flexseq_build.default(entries, monoids = out_monoids)
}

#' @method as_flexseq priority_queue
#' @export
# Runtime: O(n) from list materialization + linear rebuild.
as_flexseq.priority_queue <- function(x) {
  .as_flexseq_build.priority_queue(x, monoids = NULL)
}

#' Plot a Priority Queue Tree
#'
#' @method plot priority_queue
#' @param x A `priority_queue`.
#' @param ... Passed to the internal tree plotting routine.
#' @export
# Runtime: O(n) to build plot graph data.
plot.priority_queue <- function(x, ...) {
  plot.flexseq(x, ...)
}

#' Priority Queue Length
#'
#' @method length priority_queue
#' @param x A `priority_queue`.
#' @return Integer length.
#' @export
length.priority_queue <- function(x) {
  as.integer(node_measure(x, ".size"))
}
