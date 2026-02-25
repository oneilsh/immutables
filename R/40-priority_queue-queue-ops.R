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
