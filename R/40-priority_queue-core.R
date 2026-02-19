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
.pq_parse_entry <- function(entry, context = "priority_queue") {
  if(!is.list(entry)) {
    stop(context, " entries must be named lists with fields: item, priority (optional: name).")
  }
  nm <- names(entry)
  if(is.null(nm) || any(is.na(nm)) || any(nm == "")) {
    stop(context, " entries must be named lists with fields: item, priority (optional: name).")
  }
  if(anyDuplicated(nm) > 0L) {
    stop(context, " entry fields must be unique.")
  }
  bad <- setdiff(nm, c("item", "priority", "name", "seq_id"))
  if(length(bad) > 0L) {
    stop(context, " entry contains unsupported field(s): ", paste(bad, collapse = ", "))
  }
  if(!("item" %in% nm) || !("priority" %in% nm)) {
    stop(context, " entries must include both `item` and `priority`.")
  }

  out <- list(
    item = entry[["item"]],
    priority = .pq_assert_priority(entry[["priority"]])
  )

  nm_hint <- if("name" %in% nm) .ft_normalize_name(entry[["name"]]) else NULL
  if(is.null(nm_hint)) {
    nm_hint <- .ft_get_name(entry)
  }
  .ft_set_name(out, nm_hint)
}

# Runtime: O(n) to validate all entries.
.pq_validate_tree_entries <- function(x, context = "priority_queue") {
  els <- .ft_to_list(x)
  if(length(els) == 0L) {
    return(invisible(TRUE))
  }
  for(el in els) {
    .pq_parse_entry(el, context = context)
  }
  invisible(TRUE)
}

# Runtime: O(n) to validate then O(1) to reclass.
.pq_restore_tree <- function(x, context = "priority_queue") {
  .pq_validate_tree_entries(x, context = context)
  .as_priority_queue(x)
}

# Runtime: O(1).
.pq_make_entry <- function(item, priority) {
  list(item = item, priority = .pq_assert_priority(priority))
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
.as_priority_queue <- function(x) {
  if(!is_structural_node(x)) {
    stop("Expected a structural tree node.")
  }
  class(x) <- unique(c("priority_queue", "flexseq", setdiff(class(x), "list")))
  attr(x, "pq_next_seq") <- NULL
  x
}

# Runtime: O(log n) near right edge, with O(1) local name-state checks.
.pq_append_entry <- function(q, entry) {
  ms <- attr(q, "monoids", exact = TRUE)
  if(is.null(ms)) {
    stop("Tree has no monoids attribute.")
  }
  m <- attr(q, "measures", exact = TRUE)
  if(is.null(m)) {
    stop("Tree has no measures attribute.")
  }

  n <- as.integer(m[[".size"]])
  nn <- as.integer(m[[".named_count"]])
  if(n > 0L && nn != 0L && nn != n) {
    stop("Invalid tree name state: mixed named/unnamed elements.")
  }

  nm <- .ft_get_name(entry)
  if(nn == 0L) {
    if(!is.null(nm) && n > 0L) {
      stop("Cannot mix named and unnamed elements (insert would create mixed named and unnamed tree).")
    }
    if(.ft_cpp_can_use(ms)) {
      out <- if(is.null(nm)) .ft_cpp_add_right(q, entry, ms) else .ft_cpp_add_right_named(q, entry, nm, ms)
      return(.as_priority_queue(out))
    }
    entry2 <- if(is.null(nm)) entry else .ft_set_name(entry, nm)
    return(.as_priority_queue(.add_right_fast(q, entry2, ms)))
  }

  if(is.null(nm)) {
    stop("Cannot mix named and unnamed elements (insert would create mixed named and unnamed tree).")
  }
  if(.ft_cpp_can_use(ms)) {
    return(.as_priority_queue(.ft_cpp_add_right_named(q, entry, nm, ms)))
  }
  .as_priority_queue(.add_right_fast(q, .ft_set_name(entry, nm), ms))
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

  entries <- vector("list", n)
  for(i in seq_len(n)) {
    entries[[i]] <- .pq_make_entry(x_list[[i]], p_list[[i]])
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
  .as_priority_queue(q)
}

# Runtime: O(n log n) from underlying sequence construction.
#' Construct a Priority Queue
#'
#' Priority queues expose queue-oriented operations (`insert`, `peek_*`,
#' `pop_*`, and `lapply`). For full sequence-style editing and traversal,
#' cast explicitly with `as_flexseq()`.
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
    return(.as_priority_queue(as_flexseq(list(), monoids = .pq_merge_monoids(monoids))))
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
  entry <- .pq_make_entry(element, priority)

  if(!is.null(name)) {
    entry <- .ft_set_name(entry, name)
  }
  .pq_append_entry(q, entry)
}

# Runtime: O(log n) near locate point depth.
.pq_peek <- function(q, monoid_name) {
  .pq_assert_queue(q)
  if(length(q) == 0L) {
    stop("Cannot peek from an empty priority_queue.")
  }

  target <- node_measure(q, monoid_name)
  pred <- function(v) .pq_measure_equal(v, target)
  ctx <- resolve_named_monoid(q, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid
  loc <- if(.ft_cpp_can_use(ms)) {
    .ft_cpp_locate(q, pred, ms, monoid_name, mr$i)
  } else {
    locate_tree_impl_fast(pred, mr$i, q, ms, mr, monoid_name, 0L)
  }
  loc$elem[["item"]]
}

# Runtime: O(log n) near split point depth.
.pq_extract <- function(q, monoid_name) {
  .pq_assert_queue(q)
  if(length(q) == 0L) {
    stop("Cannot pop from an empty priority_queue.")
  }

  target <- node_measure(q, monoid_name)
  pred <- function(v) .pq_measure_equal(v, target)
  ctx <- resolve_named_monoid(q, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid
  s <- if(.ft_cpp_can_use(ms)) {
    .ft_cpp_split_tree(q, pred, ms, monoid_name, mr$i)
  } else {
    split_tree_impl_fast(pred, mr$i, q, ms, mr, monoid_name)
  }

  rest <- concat_trees(s$left, s$right)
  rest <- .as_priority_queue(rest)

  list(
    element = s$elem[["item"]],
    priority = as.numeric(s$elem[["priority"]]),
    remaining = rest
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
#' Pop minimum-priority element
#'
#' @param q A `priority_queue`.
#' @return List with `element`, `priority`, and updated `remaining`.
#' @examples
#' x <- priority_queue("a", "b", "c", priorities = c(2, 1, 1))
#' out <- pop_min(x)
#' out$element
#' out$priority
#' out$remaining
#' @export
pop_min <- function(q) {
  .pq_extract(q, ".pq_min")
}

# Runtime: O(log n) near split point depth.
#' Pop maximum-priority element
#'
#' @param q A `priority_queue`.
#' @return List with `element`, `priority`, and updated `remaining`.
#' @examples
#' x <- priority_queue("a", "b", "c", priorities = c(2, 3, 3))
#' out <- pop_max(x)
#' out$element
#' out$priority
#' out$remaining
#' @export
pop_max <- function(q) {
  .pq_extract(q, ".pq_max")
}

#' @rdname length.flexseq
#' @method length priority_queue
#' @export
length.priority_queue <- function(x) {
  as.integer(node_measure(x, ".size"))
}

# Runtime: O(n log n) total from entry traversal + queue rebuild.
.pq_apply_impl <- function(q, f, ...) {
  .pq_assert_queue(q)
  if(!is.function(f)) {
    stop("`f` must be a function.")
  }

  entries <- as.list(q)
  n <- length(entries)
  out <- vector("list", n)
  out_names <- if(is.null(names(entries))) rep("", n) else names(entries)

  for(i in seq_len(n)) {
    e <- entries[[i]]
    cur_name <- out_names[[i]]

    upd <- f(e$item, e$priority, cur_name, ...)
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
      priority = as.numeric(pr2)
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
  .as_priority_queue(q2)
}

#' Lapply over priority queue entries
#'
#' @rdname lapply
#' @method lapply priority_queue
#' @export
lapply.priority_queue <- function(X, FUN, ...) {
  if(!is.function(FUN)) {
    stop("`FUN` must be a function.")
  }
  .pq_apply_impl(X, FUN, ...)
}
