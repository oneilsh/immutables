#' Print a Priority Queue
#'
#' @param x A `priority_queue`.
#' @param ... Unused.
#' @return `x` invisibly.
#' @examples
#' x <- priority_queue("a", "b", "c", priorities = c(2, 1, 3))
#' x
#' @export
# Runtime: O(log n) for min/max summary lookups.
print.priority_queue <- function(x, ...) {
  n <- as.integer(node_measure(x, ".size"))
  cat(sprintf("<priority_queue> size=%d\n", n))
  if(n > 0L) {
    minm <- node_measure(x, ".pq_min")
    maxm <- node_measure(x, ".pq_max")
    cat(sprintf("min_priority=%s max_priority=%s\n", format(minm$priority), format(maxm$priority)))
  }
  invisible(x)
}
