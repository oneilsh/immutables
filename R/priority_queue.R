# Runtime: O(1).
.pq_assert_priority <- function(priority) {
  if(!is.numeric(priority) || length(priority) != 1L || is.na(priority)) {
    stop("`priority` must be a single non-missing numeric value.")
  }
  as.numeric(priority)
}

# Runtime: O(1).
.pq_assert_queue <- function(q) {
  if(!inherits(q, "priority_queue") || !is_structural_node(q)) {
    stop("`q` must be a priority_queue.")
  }
  invisible(TRUE)
}

# Runtime: O(1).
.pq_next_seq <- function(q) {
  n <- attr(q, "pq_next_seq", exact = TRUE)
  if(is.null(n) || !is.numeric(n) || length(n) != 1L || is.na(n)) {
    stop("priority_queue is missing valid `pq_next_seq` attribute.")
  }
  as.numeric(n)
}

# Runtime: O(1).
.pq_set_next_seq <- function(q, next_seq) {
  attr(q, "pq_next_seq") <- as.numeric(next_seq)
  q
}

# Runtime: O(1).
.pq_make_entry <- function(item, priority, seq_id) {
  list(item = item, priority = .pq_assert_priority(priority), seq_id = as.numeric(seq_id))
}

# Runtime: O(1).
.pq_decode_item <- function(entry) {
  .pq_assert_entry(entry)
  entry[["item"]]
}

# Runtime: O(1).
.pq_decode_priority <- function(entry) {
  .pq_assert_entry(entry)
  as.numeric(entry[["priority"]])
}

# Runtime: O(m), m = number of user monoids.
.pq_merge_monoids <- function(monoids = NULL) {
  user <- if(is.null(monoids)) list() else monoids
  if(length(user) > 0L) {
    bad <- intersect(names(user), c(".size", ".named_count", ".pq_min", ".pq_max"))
    if(length(bad) > 0L) {
      stop(paste0("Reserved monoid names cannot be supplied for priority queues: ", paste(bad, collapse = ", ")))
    }
  }
  c(user, .pq_required_monoids())
}

# Runtime: O(1).
.as_priority_queue <- function(x, next_seq = NULL) {
  if(!is_structural_node(x)) {
    stop("Expected a structural tree node.")
  }
  class(x) <- unique(c("priority_queue", "flexseq", setdiff(class(x), "list")))
  if(is.null(next_seq)) {
    next_seq <- as.numeric(node_measure(x, ".size")) + 1
  }
  .pq_set_next_seq(x, next_seq)
}

# Runtime: O(n log n) from underlying sequence construction.
#' Build a Priority Queue from elements and priorities
#'
#' @param x Elements to enqueue.
#' @param priorities Numeric priorities (same length as `x`).
#' @param names Optional element names for name-based indexing.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#' @return A `priority_queue`.
#' @examples
#' q <- as_priority_queue(letters[1:4], priorities = c(3, 1, 2, 1))
#' peek_min(q)
#' peek_max(q)
#' @export
as_priority_queue <- function(x, priorities, names = NULL, monoids = NULL) {
  x_list <- as.list(x)
  n <- length(x_list)

  p_list <- as.list(priorities)
  if(length(p_list) != n) {
    stop("`priorities` length must match elements length.")
  }

  for(i in seq_len(n)) {
    p_list[[i]] <- .pq_assert_priority(p_list[[i]])
  }

  entries <- vector("list", n)
  for(i in seq_len(n)) {
    entries[[i]] <- .pq_make_entry(x_list[[i]], p_list[[i]], i)
  }

  nm <- names
  if(is.null(nm)) {
    nm <- names(x)
  }
  if(!is.null(nm) && length(nm) > 0L) {
    if(length(nm) != n) {
      stop("`names` length must match elements length.")
    }
    names(entries) <- nm
  }

  q <- as_flexseq(entries, monoids = .pq_merge_monoids(monoids))
  .as_priority_queue(q, next_seq = n + 1)
}

# Runtime: O(n log n) from underlying sequence construction.
#' Construct a Priority Queue
#'
#' @param ... Elements to enqueue.
#' @param priorities Numeric priorities matching `...`.
#' @param names Optional element names.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#' @return A `priority_queue`.
#' @examples
#' q <- priority_queue("a", "b", "c", priorities = c(2, 1, 2))
#' peek_min(q)
#' @export
priority_queue <- function(..., priorities = NULL, names = NULL, monoids = NULL) {
  xs <- list(...)
  n <- length(xs)

  if(n == 0L) {
    if(!is.null(priorities) && length(priorities) > 0L) {
      stop("`priorities` must be empty when no elements are supplied.")
    }
    return(.as_priority_queue(
      as_flexseq(list(), monoids = .pq_merge_monoids(monoids)),
      next_seq = 1
    ))
  }

  if(is.null(priorities)) {
    stop("`priorities` is required when elements are supplied.")
  }

  if(!is.null(names)) {
    if(length(names) != n) {
      stop("`names` length must match number of elements.")
    }
    names(xs) <- names
  }

  as_priority_queue(xs, priorities = priorities, monoids = monoids)
}

# Runtime: O(log n) near right edge.
#' Insert an element into a priority queue
#'
#' @param q A `priority_queue`.
#' @param element Element to insert.
#' @param priority Numeric scalar priority.
#' @param name Optional element name.
#' @return Updated `priority_queue`.
#' @examples
#' q <- priority_queue("a", "b", priorities = c(2, 1))
#' q2 <- insert(q, "c", priority = 1)
#' peek_min(q2)
#' @export
insert <- function(q, element, priority, name = NULL) {
  .pq_assert_queue(q)
  seq_id <- .pq_next_seq(q)
  entry <- .pq_make_entry(element, priority, seq_id)

  if(!is.null(name)) {
    entry <- .ft_set_name(entry, name)
  }

  q2 <- append(q, entry)
  .as_priority_queue(q2, next_seq = seq_id + 1)
}

# Runtime: O(1).
#' Check whether a priority queue is empty
#'
#' @param q A `priority_queue`.
#' @return Logical scalar.
#' @examples
#' q <- priority_queue()
#' is_empty(q)
#' @export
is_empty <- function(q) {
  .pq_assert_queue(q)
  as.integer(node_measure(q, ".size")) == 0L
}

# Runtime: O(log n) near locate point depth.
.pq_peek <- function(q, monoid_name) {
  .pq_assert_queue(q)
  if(is_empty(q)) {
    stop("Cannot peek from an empty priority_queue.")
  }

  target <- node_measure(q, monoid_name)
  pred <- function(v) .pq_measure_equal(v, target)
  loc <- locate(q, pred, monoid_name)
  .pq_decode_item(loc$elem)
}

# Runtime: O(log n) near split point depth.
.pq_extract <- function(q, monoid_name) {
  .pq_assert_queue(q)
  if(is_empty(q)) {
    stop("Cannot extract from an empty priority_queue.")
  }

  target <- node_measure(q, monoid_name)
  pred <- function(v) .pq_measure_equal(v, target)
  s <- split_tree(q, pred, monoid_name)

  rest <- concat_trees(s$left, s$right)
  rest <- .as_priority_queue(rest, next_seq = .pq_next_seq(q))

  list(
    element = .pq_decode_item(s$elem),
    priority = .pq_decode_priority(s$elem),
    queue = rest
  )
}

# Runtime: O(log n) near locate point depth.
#' Peek minimum-priority element
#'
#' @param q A `priority_queue`.
#' @return Element with minimum priority (stable on ties).
#' @examples
#' q <- priority_queue("a", "b", "c", priorities = c(2, 1, 1))
#' peek_min(q)
#' @export
peek_min <- function(q) {
  .pq_peek(q, ".pq_min")
}

# Runtime: O(log n) near locate point depth.
#' Peek maximum-priority element
#'
#' @param q A `priority_queue`.
#' @return Element with maximum priority (stable on ties).
#' @examples
#' q <- priority_queue("a", "b", "c", priorities = c(2, 3, 3))
#' peek_max(q)
#' @export
peek_max <- function(q) {
  .pq_peek(q, ".pq_max")
}

# Runtime: O(log n) near split point depth.
#' Extract minimum-priority element
#'
#' @param q A `priority_queue`.
#' @return List with `element`, `priority`, and updated `queue`.
#' @examples
#' q <- priority_queue("a", "b", "c", priorities = c(2, 1, 1))
#' out <- extract_min(q)
#' out$element
#' out$priority
#' @export
extract_min <- function(q) {
  .pq_extract(q, ".pq_min")
}

# Runtime: O(log n) near split point depth.
#' Extract maximum-priority element
#'
#' @param q A `priority_queue`.
#' @return List with `element`, `priority`, and updated `queue`.
#' @examples
#' q <- priority_queue("a", "b", "c", priorities = c(2, 3, 3))
#' out <- extract_max(q)
#' out$element
#' out$priority
#' @export
extract_max <- function(q) {
  .pq_extract(q, ".pq_max")
}

#' Priority Queue Length
#'
#' @method length priority_queue
#' @param x A `priority_queue`.
#' @return Number of entries.
#' @export
# Runtime: O(1) from cached `.size`.
length.priority_queue <- function(x) {
  as.integer(node_measure(x, ".size"))
}
