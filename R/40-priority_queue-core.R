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
.pq_make_entry <- function(item, priority, seq_id) {
  list(item = item, priority = .pq_assert_priority(priority), seq_id = as.numeric(seq_id))
}

# Runtime: O(1) under fixed monoid set.
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
  attr(x, "pq_next_seq") <- as.numeric(next_seq)
  x
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
#' x <- as_priority_queue(letters[1:4], priorities = c(3, 1, 2, 1))
#' x
#' peek_min(x)
#' peek_max(x)
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
#' x <- priority_queue("a", "b", "c", priorities = c(2, 1, 2))
#' x
#' peek_min(x)
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

# Runtime: O(1).
#' Insert an element
#'
#' Generic `insert()` dispatches by class.
#'
#' @title Insert an element
#' @param x Object to insert into.
#' @param ... Method-specific arguments.
#' @return Updated object.
#' @export
insert <- function(x, ...) {
  UseMethod("insert")
}

#' @export
#' @noRd
insert.default <- function(x, ...) {
  cls <- class(x)
  cls_txt <- if(length(cls) == 0L) "unknown" else paste(cls, collapse = "/")
  stop(sprintf("No `insert()` method for class '%s'.", cls_txt))
}

# Runtime: O(log n) near right edge.
#' Insert an element into a priority queue
#'
#' @rdname insert
#' @param element Element to insert.
#' @param priority Numeric scalar priority.
#' @param name Optional element name.
#' @examples
#' x <- priority_queue("a", "b", priorities = c(2, 1))
#' x
#'
#' x2 <- insert(x, "c", priority = 1)
#' x2
#' peek_min(x2)
#' @export
insert.priority_queue <- function(x, element, priority, name = NULL, ...) {
  q <- x
  .pq_assert_queue(q)
  seq_id <- .pq_next_seq(q)
  entry <- .pq_make_entry(element, priority, seq_id)

  if(!is.null(name)) {
    entry <- .ft_set_name(entry, name)
  }

  q2 <- append(q, entry)
  .as_priority_queue(q2, next_seq = seq_id + 1)
}

# Runtime: O(log n) near locate point depth.
.pq_peek <- function(q, monoid_name) {
  .pq_assert_queue(q)
  if(is_empty(q)) {
    stop("Cannot peek from an empty priority_queue.")
  }

  target <- node_measure(q, monoid_name)
  pred <- function(v) .pq_measure_equal(v, target)
  loc <- locate_by_predicate(q, pred, monoid_name)
  loc$elem[["item"]]
}

# Runtime: O(log n) near split point depth.
.pq_extract <- function(q, monoid_name) {
  .pq_assert_queue(q)
  if(is_empty(q)) {
    stop("Cannot extract from an empty priority_queue.")
  }

  target <- node_measure(q, monoid_name)
  pred <- function(v) .pq_measure_equal(v, target)
  s <- split_around_by_predicate(q, pred, monoid_name)

  rest <- concat_trees(s$left, s$right)
  rest <- .as_priority_queue(rest, next_seq = .pq_next_seq(q))

  list(
    element = s$elem[["item"]],
    priority = as.numeric(s$elem[["priority"]]),
    queue = rest
  )
}

# Runtime: O(log n) near locate point depth.
#' Peek minimum-priority element
#'
#' @param q A `priority_queue`.
#' @return Element with minimum priority (stable on ties).
#' @examples
#' x <- priority_queue("a", "b", "c", priorities = c(2, 1, 1))
#' x
#' peek_min(x)
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
#' x <- priority_queue("a", "b", "c", priorities = c(2, 3, 3))
#' x
#' peek_max(x)
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
#' x <- priority_queue("a", "b", "c", priorities = c(2, 1, 1))
#' out <- extract_min(x)
#' out$element
#' out$priority
#' out$queue
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
#' x <- priority_queue("a", "b", "c", priorities = c(2, 3, 3))
#' out <- extract_max(x)
#' out$element
#' out$priority
#' out$queue
#' @export
extract_max <- function(q) {
  .pq_extract(q, ".pq_max")
}

# Runtime: O(n log n) total from entry traversal + queue rebuild.
.pq_apply_impl <- function(q, f, reset_ties = TRUE, ...) {
  .pq_assert_queue(q)
  if(!is.function(f)) {
    stop("`f` must be a function.")
  }
  if(!is.logical(reset_ties) || length(reset_ties) != 1L || is.na(reset_ties)) {
    stop("`reset_ties` must be TRUE or FALSE.")
  }

  entries <- as.list(q)
  n <- length(entries)
  out <- vector("list", n)
  out_names <- if(is.null(names(entries))) rep("", n) else names(entries)

  for(i in seq_len(n)) {
    e <- entries[[i]]
    cur_name <- out_names[[i]]

    upd <- f(e$item, e$priority, e$seq_id, cur_name, ...)
    if(!is.list(upd)) {
      stop("`f` must return a list.")
    }
    if(length(upd) > 0L) {
      nm <- names(upd)
      if(is.null(nm) || any(is.na(nm)) || any(nm == "")) {
        stop("`f` must return a named list using only: item, priority, name.")
      }
      if(anyDuplicated(nm) > 0L) {
        stop("`f` return list cannot contain duplicated field names.")
      }
      bad <- setdiff(nm, c("item", "priority", "name"))
      if(length(bad) > 0L) {
        stop("`f` returned unsupported field(s): ", paste(bad, collapse = ", "))
      }
    }

    item2 <- if("item" %in% names(upd)) upd[["item"]] else e$item
    pr2 <- if("priority" %in% names(upd)) .pq_assert_priority(upd[["priority"]]) else as.numeric(e$priority)
    if("name" %in% names(upd)) {
      nm2 <- .ft_normalize_name(upd[["name"]])
      out_names[[i]] <- if(is.null(nm2)) "" else nm2
    }

    out[[i]] <- list(
      item = item2,
      priority = as.numeric(pr2),
      seq_id = if(isTRUE(reset_ties)) as.numeric(i) else as.numeric(e$seq_id)
    )
  }

  if(any(out_names != "")) {
    names(out) <- out_names
  }

  all_monoids <- attr(q, "monoids", exact = TRUE)
  user_monoids <- all_monoids[setdiff(names(all_monoids), c(".size", ".named_count", ".pq_min", ".pq_max"))]
  if(length(user_monoids) == 0L) {
    user_monoids <- NULL
  }

  q2 <- as_flexseq(out, monoids = .pq_merge_monoids(user_monoids))
  next_seq <- if(isTRUE(reset_ties)) as.numeric(n + 1L) else .pq_next_seq(q)
  .as_priority_queue(q2, next_seq = next_seq)
}

#' Apply over priority queue entries
#'
#' @rdname apply
#' @method apply priority_queue
#' @param reset_ties Logical; if `TRUE`, refreshes tie-break `seq_id` by
#'   current object order. If `FALSE`, preserves existing `seq_id` values.
#' @export
apply.priority_queue <- function(X, MARGIN = NULL, FUN = NULL, ..., reset_ties = TRUE) {
  if(is.null(FUN)) {
    if(is.function(MARGIN)) {
      FUN <- MARGIN
      MARGIN <- NULL
    } else {
      stop("`FUN` must be a function.")
    }
  }
  if(!is.null(MARGIN)) {
    stop("`MARGIN` is not used for priority_queue; call `apply(x, FUN, ...)`.")
  }
  .pq_apply_impl(X, FUN, reset_ties = reset_ties, ...)
}
