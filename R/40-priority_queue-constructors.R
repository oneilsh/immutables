# Runtime: O(n) overall (entry normalization + linear sequence construction).
#' Build a Priority Queue from elements and priorities
#'
#' @param x Elements to enqueue.
#' @param priorities Scalar non-missing orderable priorities (same length as `x`).
#' @return A `priority_queue`.
#' @examples
#' x <- as_priority_queue(letters[1:4], priorities = c(3, 1, 2, 1))
#' x
#' peek_min(x)
#' peek_max(x)
#' @export
as_priority_queue <- function(x, priorities) {
  .as_priority_queue_build(x, priorities = priorities, monoids = NULL)
}

# Runtime: O(n) overall (entry normalization + linear sequence construction).
.as_priority_queue_build <- function(x, priorities, monoids = NULL) {
  x_list <- as.list(x)
  n <- length(x_list)

  p_list <- as.list(priorities)
  if(length(p_list) != n) {
    stop("`priorities` length must match elements length.")
  }

  entries <- vector("list", n)
  priority_type <- NULL
  for(i in seq_len(n)) {
    parsed <- .pq_make_entry(x_list[[i]], p_list[[i]], priority_type = priority_type)
    priority_type <- parsed$priority_type
    entries[[i]] <- parsed$entry
  }

  nm <- names(x)
  if(!is.null(nm) && length(nm) > 0L) {
    if(length(nm) != n) {
      stop("`names` length must match elements length.")
    }
    names(entries) <- nm
  }

  q <- .as_flexseq_build(entries, monoids = .pq_merge_monoids(monoids))
  .as_priority_queue(q, priority_type = priority_type)
}

# Runtime: O(n) overall (entry normalization + linear sequence construction).
#' Construct a Priority Queue
#'
#' Priority queues expose queue-oriented operations (`insert`, `peek_*`,
#' `pop_*`, and `fapply`). For full sequence-style editing and traversal,
#' cast explicitly with `as_flexseq()`.
#'
#' @param ... Elements to enqueue.
#' @param priorities Scalar non-missing orderable priorities matching `...`.
#' @return A `priority_queue`.
#' @examples
#' x <- priority_queue("a", "b", "c", priorities = c(2, 1, 2))
#' x
#' peek_min(x)
#' @export
priority_queue <- function(..., priorities) {
  if(missing(priorities)) {
    priorities <- NULL
  }
  .priority_queue_build(..., priorities = priorities, monoids = NULL)
}

# Runtime: O(n) overall (entry normalization + linear sequence construction).
.priority_queue_build <- function(..., priorities = NULL, monoids = NULL) {
  xs <- list(...)
  n <- length(xs)

  if(n == 0L) {
    if(!is.null(priorities) && length(priorities) > 0L) {
      stop("`priorities` must be empty when no elements are supplied.")
    }
    return(.as_priority_queue(.as_flexseq_build(list(), monoids = .pq_merge_monoids(monoids)), priority_type = NULL))
  }

  if(is.null(priorities)) {
    stop("`priorities` is required when elements are supplied.")
  }

  .as_priority_queue_build(xs, priorities = priorities, monoids = monoids)
}
