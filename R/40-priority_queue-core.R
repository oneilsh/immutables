# Runtime: O(1).
.pq_priority_type_state <- function(x) {
  attr(x, "pq_priority_type", exact = TRUE)
}

# Runtime: O(1).
.pq_validate_priority_type <- function(pq_priority_type, new_priority_type) {
  if(is.null(pq_priority_type)) {
    return(new_priority_type)
  }
  if(!identical(pq_priority_type, new_priority_type)) {
    stop("Incompatible priority type for this priority_queue.")
  }
  pq_priority_type
}

# Runtime: O(1).
.pq_normalize_priority <- function(priority) {
  norm <- .ft_normalize_scalar_orderable(priority, arg_name = "priority")
  list(priority = norm$value, priority_type = norm$value_type)
}

# Runtime: O(1).
.pq_assert_queue <- function(q) {
  if(!inherits(q, "priority_queue") || !is_structural_node(q)) {
    stop("`q` must be a priority_queue.")
  }
  invisible(TRUE)
}

# Runtime: O(n) over tree size for any non-trivial update (rebind/recompute pass).
#' @method add_monoids priority_queue
#' @export
#' @noRd
add_monoids.priority_queue <- function(t, monoids, overwrite = FALSE) {
  if(length(monoids) > 0L) {
    bad <- intersect(names(monoids), c(".size", ".named_count", ".pq_min", ".pq_max"))
    if(length(bad) > 0L) {
      stop("Reserved monoid names cannot be supplied for priority_queue: ", paste(bad, collapse = ", "))
    }
  }
  add_monoids.flexseq(t, monoids, overwrite = overwrite)
}

#' Indexing for Priority Queues
#'
#' Name-based indexing is supported for reads only. Positional indexing and all
#' replacement indexing are intentionally blocked to preserve queue-first UX.
#'
#' @name sub-.priority_queue
#' @param x A `priority_queue`.
#' @param i Index input. For reads, must be a character name (scalar for `[[`).
#' @param value Replacement value (unsupported).
#' @param ... Unused.
#' @return For `$`/`[[`/`[`: queue payload values or queue subsets by name.
#'   Replacement forms always error.
NULL

# Runtime: O(k * n_lookup) for short name queries; O(n + k) in map-backed paths.
#' @rdname sub-.priority_queue
#' @method [ priority_queue
#' @export
`[.priority_queue` <- function(x, i, ...) {
  if(missing(i)) {
    return(x)
  }
  if(!is.character(i)) {
    stop("`[.priority_queue` supports character name indexing only. Cast first with `as_flexseq()`.")
  }
  `[.flexseq`(x, i, ...)
}

# Runtime: O(n_lookup) single name lookup + O(log n) element fetch.
#' @rdname sub-.priority_queue
#' @method [[ priority_queue
#' @export
`[[.priority_queue` <- function(x, i, ...) {
  if(!(is.character(i) && length(i) == 1L && !is.na(i))) {
    stop("`[[.priority_queue` supports scalar character names only. Cast first with `as_flexseq()`.")
  }
  entry <- `[[.flexseq`(x, i, ...)
  if(!is.list(entry) || !("item" %in% names(entry))) {
    stop("Malformed priority_queue entry.")
  }
  entry$item
}

# Runtime: O(1).
#' @rdname sub-.priority_queue
#' @method [<- priority_queue
#' @export
`[<-.priority_queue` <- function(x, i, value) {
  stop("`[<-` is not supported for priority_queue. Cast first with `as_flexseq()`.")
}

# Runtime: O(1).
#' @rdname sub-.priority_queue
#' @method [[<- priority_queue
#' @export
`[[<-.priority_queue` <- function(x, i, value) {
  stop("`[[<-` is not supported for priority_queue. Cast first with `as_flexseq()`.")
}

# Runtime: O(1).
#' @method split_by_predicate priority_queue
#' @export
#' @noRd
split_by_predicate.priority_queue <- function(x, predicate, monoid_name) {
  stop("`split_by_predicate()` is not supported for priority_queue. Cast first with `as_flexseq()`.")
}

# Runtime: O(1).
#' @method split_around_by_predicate priority_queue
#' @export
#' @noRd
split_around_by_predicate.priority_queue <- function(t, predicate, monoid_name, accumulator = NULL) {
  stop("`split_around_by_predicate()` is not supported for priority_queue. Cast first with `as_flexseq()`.")
}

# Runtime: O(1).
#' @method split_at priority_queue
#' @export
#' @noRd
split_at.priority_queue <- function(x, at, pull_index = FALSE) {
  stop("`split_at()` is not supported for priority_queue. Cast first with `as_flexseq()`.")
}

# Runtime: O(1).
.pq_parse_entry <- function(entry, context = "priority_queue", priority_type = NULL) {
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

  norm <- .pq_normalize_priority(entry[["priority"]])
  resolved_type <- .pq_validate_priority_type(priority_type, norm$priority_type)
  out <- list(item = entry[["item"]], priority = norm$priority)

  nm_hint <- if("name" %in% nm) .ft_normalize_name(entry[["name"]]) else NULL
  if(is.null(nm_hint)) {
    nm_hint <- .ft_get_name(entry)
  }
  list(entry = .ft_set_name(out, nm_hint), priority_type = resolved_type)
}

# Runtime: O(n) to validate all entries.
.pq_validate_tree_entries <- function(x, context = "priority_queue", priority_type = NULL) {
  els <- .ft_to_list(x)
  if(length(els) == 0L) {
    return(priority_type)
  }
  out_type <- priority_type
  for(el in els) {
    parsed <- .pq_parse_entry(el, context = context, priority_type = out_type)
    out_type <- parsed$priority_type
  }
  out_type
}

# Runtime: O(n) to validate then O(1) to reclass.
.pq_restore_tree <- function(x, template = NULL, context = "priority_queue") {
  priority_type <- if(is.null(template)) NULL else .pq_priority_type_state(template)
  priority_type <- .pq_validate_tree_entries(x, context = context, priority_type = priority_type)
  .as_priority_queue(x, priority_type = priority_type)
}

# Runtime: O(1).
.pq_make_entry <- function(item, priority, priority_type = NULL) {
  norm <- .pq_normalize_priority(priority)
  list(
    entry = list(item = item, priority = norm$priority),
    priority_type = .pq_validate_priority_type(priority_type, norm$priority_type)
  )
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
.as_priority_queue <- function(x, priority_type = NULL) {
  if(!is_structural_node(x)) {
    stop("Expected a structural tree node.")
  }
  class(x) <- unique(c("priority_queue", "flexseq", setdiff(class(x), "list")))
  attr(x, "pq_next_seq") <- NULL
  attr(x, "pq_priority_type") <- priority_type
  x
}

# Runtime: O(1).
.pq_wrap_like <- function(template, tree, priority_type = NULL) {
  resolved_type <- if(is.null(priority_type)) .pq_priority_type_state(template) else priority_type
  .as_priority_queue(tree, priority_type = resolved_type)
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
      return(.pq_wrap_like(q, out))
    }
    entry2 <- if(is.null(nm)) entry else .ft_set_name(entry, nm)
    return(.pq_wrap_like(q, add_right(q, entry2, ms)))
  }

  if(is.null(nm)) {
    stop("Cannot mix named and unnamed elements (insert would create mixed named and unnamed tree).")
  }
  if(.ft_cpp_can_use(ms)) {
    return(.pq_wrap_like(q, .ft_cpp_add_right_named(q, entry, nm, ms)))
  }
  .pq_wrap_like(q, add_right(q, .ft_set_name(entry, nm), ms))
}

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

# Runtime: O(log n) near right edge.
#' Insert an element into a priority queue
#'
#' @method insert priority_queue
#' @param x A `priority_queue`.
#' @param element Element to insert.
#' @param priority Scalar non-missing orderable priority.
#' @param name Optional element name.
#' @param ... Unused.
#' @return Updated `priority_queue`.
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
  parsed <- .pq_make_entry(element, priority, priority_type = .pq_priority_type_state(q))
  entry <- parsed$entry

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
  rest <- .pq_wrap_like(q, rest)

  list(
    element = s$elem[["item"]],
    priority = s$elem[["priority"]],
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

#' Priority Queue Length
#'
#' @method length priority_queue
#' @param x A `priority_queue`.
#' @return Integer length.
#' @export
length.priority_queue <- function(x) {
  as.integer(node_measure(x, ".size"))
}

# Runtime: O(n) total from entry traversal + linear queue rebuild.
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

    item2 <- f(e$item, e$priority, cur_name, ...)

    out[[i]] <- list(
      item = item2,
      priority = e$priority
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

  q2 <- .as_flexseq_build(out, monoids = .pq_merge_monoids(user_monoids))
  .pq_wrap_like(q, q2)
}

#' Apply a function over priority queue entries
#'
#' @method fapply priority_queue
#' @param X A `priority_queue`.
#' @param FUN Function of `(item, priority, name, ...)` returning the new
#'   payload item. Queue metadata (`priority`, `name`) is read-only.
#' @param ... Additional arguments passed to `FUN`.
#' @return A new `priority_queue` with transformed entries.
#' @export
fapply.priority_queue <- function(X, FUN, ...) {
  if(!is.function(FUN)) {
    stop("`FUN` must be a function.")
  }
  .pq_apply_impl(X, FUN, ...)
}
