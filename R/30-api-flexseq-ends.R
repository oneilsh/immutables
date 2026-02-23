# Runtime: O(log n) tree update, with O(1) local name-state checks.
.ft_push_back_impl <- function(x, value, context = "push_back()") {
  ms <- attr(x, "monoids", exact = TRUE)
  if(is.null(ms)) {
    stop("Tree has no monoids attribute.")
  }
  m <- attr(x, "measures", exact = TRUE)
  if(is.null(m)) {
    stop("Tree has no measures attribute.")
  }
  n <- as.integer(m[[".size"]])
  nn <- as.integer(m[[".named_count"]])
  if(n > 0L && nn != 0L && nn != n) {
    stop("Invalid tree name state: mixed named/unnamed elements.")
  }

  value2 <- value
  if(nn == 0L) {
    nm <- .ft_get_name(value2)
    if(is.null(nm)) {
      nm <- .ft_name_from_value(value2)
    }
    if(is.null(nm)) {
      if(.ft_cpp_can_use(ms)) {
        return(.ft_restore_subclass(.ft_cpp_add_right(x, value2, ms), x, context = context))
      }
      return(.ft_restore_subclass(.add_right_fast(x, value2, ms), x, context = context))
    }
    if(n > 0L) {
      stop("Cannot mix named and unnamed elements (push_back would create mixed named and unnamed tree).")
    }
    if(.ft_cpp_can_use(ms)) {
      return(.ft_restore_subclass(.ft_cpp_add_right_named(x, value2, nm, ms), x, context = context))
    }
    value2 <- .ft_set_name(value2, nm)
  } else {
    nm <- .ft_effective_name(value2)
    if(is.null(nm)) {
      stop("Cannot mix named and unnamed elements (push_back would create mixed named and unnamed tree).")
    }
    if(.ft_cpp_can_use(ms)) {
      return(.ft_restore_subclass(.ft_cpp_add_right_named(x, value2, nm, ms), x, context = context))
    }
    value2 <- .ft_set_name(value2, nm)
  }

  if(.ft_cpp_can_use(ms)) {
    return(.ft_restore_subclass(.ft_cpp_add_right(x, value2, ms), x, context = context))
  }
  .ft_restore_subclass(.add_right_fast(x, value2, ms), x, context = context)
}

#' Push an element to the back
#'
#' @param x A `flexseq`.
#' @param value Element to push.
#' @return Updated tree.
#'   If `x` is name-indexed, pushing unnamed elements is invalid.
#' @examples
#' s <- as_flexseq(letters[1:3])
#' s2 <- push_back(s, "d")
#' s2
#'
#' n <- as_flexseq(list(two = 2, three = 3))
#' new_el <- 4
#' names(new_el) <- "four"
#' push_back(n, new_el)
#' @export
# Runtime: O(log n) tree update, with O(1) local name-state checks.
push_back <- function(x, value) {
  if(inherits(x, "ordered_sequence")) {
    .ft_stop_ordered_like(x, "push_back", "Use `insert()`.")
  }
  if(inherits(x, "priority_queue")) {
    stop("`push_back()` is not supported for priority_queue. Cast first with `as_flexseq()`.")
  }
  .ft_push_back_impl(x, value, context = "push_back()")
}

#' Push an element to the front
#'
#' @param x A `flexseq`.
#' @param value Element to push.
#' @return Updated tree.
#'   If `x` is name-indexed, pushing unnamed elements is invalid.
#' @examples
#' s <- as_flexseq(letters[2:4])
#' s2 <- push_front(s, "a")
#' s2
#'
#' n <- as_flexseq(list(two = 2, three = 3))
#' new_el <- 1
#' names(new_el) <- "one"
#' push_front(n, new_el)
#' @export
# Runtime: O(log n) tree update, with O(1) local name-state checks.
push_front <- function(x, value) {
  if(inherits(x, "ordered_sequence")) {
    .ft_stop_ordered_like(x, "push_front", "Use `insert()`.")
  }
  if(inherits(x, "priority_queue")) {
    stop("`push_front()` is not supported for priority_queue. Cast first with `as_flexseq()`.")
  }

  ms <- attr(x, "monoids", exact = TRUE)
  if(is.null(ms)) {
    stop("Tree has no monoids attribute.")
  }
  m <- attr(x, "measures", exact = TRUE)
  if(is.null(m)) {
    stop("Tree has no measures attribute.")
  }
  n <- as.integer(m[[".size"]])
  nn <- as.integer(m[[".named_count"]])
  if(n > 0L && nn != 0L && nn != n) {
    stop("Invalid tree name state: mixed named/unnamed elements.")
  }

  value2 <- value
  if(nn == 0L) {
    nm <- .ft_get_name(value2)
    if(is.null(nm)) {
      nm <- .ft_name_from_value(value2)
    }
    if(is.null(nm)) {
      if(.ft_cpp_can_use(ms)) {
        return(.ft_restore_subclass(.ft_cpp_add_left(x, value2, ms), x, context = "push_front()"))
      }
      return(.ft_restore_subclass(.add_left_fast(x, value2, ms), x, context = "push_front()"))
    }
    if(n > 0L) {
      stop("Cannot mix named and unnamed elements (push_front would create mixed named and unnamed tree).")
    }
    if(.ft_cpp_can_use(ms)) {
      return(.ft_restore_subclass(.ft_cpp_add_left_named(x, value2, nm, ms), x, context = "push_front()"))
    }
    value2 <- .ft_set_name(value2, nm)
  } else {
    nm <- .ft_effective_name(value2)
    if(is.null(nm)) {
      stop("Cannot mix named and unnamed elements (push_front would create mixed named and unnamed tree).")
    }
    if(.ft_cpp_can_use(ms)) {
      return(.ft_restore_subclass(.ft_cpp_add_left_named(x, value2, nm, ms), x, context = "push_front()"))
    }
    value2 <- .ft_set_name(value2, nm)
  }

  if(.ft_cpp_can_use(ms)) {
    return(.ft_restore_subclass(.ft_cpp_add_left(x, value2, ms), x, context = "push_front()"))
  }
  .ft_restore_subclass(.add_left_fast(x, value2, ms), x, context = "push_front()")
}

# Runtime: O(1) for empty rebuild metadata, class restoration depends on type.
.ft_empty_like <- function(x, context = "pop") {
  ms <- resolve_tree_monoids(x, required = TRUE)
  e <- empty_tree(monoids = ms)
  if(inherits(x, "interval_index")) {
    return(.ivx_wrap_like(x, e))
  }
  if(inherits(x, "ordered_sequence")) {
    return(.ord_wrap_like(x, e))
  }
  .ft_restore_subclass(e, x, context = context)
}

# Runtime: O(1).
.ft_public_value <- function(x, el) {
  if(inherits(x, "interval_index")) {
    if(is.list(el) && ("item" %in% names(el))) {
      return(el$item)
    }
    return(el)
  }
  if(inherits(x, "ordered_sequence")) {
    if(is.list(el) && ("item" %in% names(el))) {
      return(el$item)
    }
    return(el)
  }
  el
}

# validate one positional index for public peek/pop-at helpers.
# Runtime: O(1).
.ft_assert_one_positional_index <- function(index, n) {
  idx <- .ft_assert_int_indices(index, n)
  if(length(idx) != 1L) {
    stop("`index` must be a single positive integer.")
  }
  idx
}

# validate one insertion index in [1, n + 1].
# Runtime: O(1).
.ft_assert_insert_index <- function(index, n) {
  idx <- .ft_assert_int_indices(index, n + 1L)
  if(length(idx) != 1L) {
    stop("`index` must be a single positive integer.")
  }
  idx
}

# normalize insert_at payload into a plain list of elements.
# Runtime: O(k), where k = number of inserted elements.
.ft_insert_values_list <- function(values) {
  if(inherits(values, "priority_queue")) {
    stop("`values` cannot be a priority_queue. Cast first with `as_flexseq()`.")
  }
  if(inherits(values, "ordered_sequence")) {
    stop(sprintf(
      "`values` cannot be an ordered sequence subclass (%s). Cast first with `as_flexseq()`.",
      .ft_ordered_owner_class(values)
    ))
  }
  if(inherits(values, "flexseq")) {
    return(as.list(values))
  }
  if(is.list(values)) {
    return(values)
  }
  as.list(values)
}

# enforce global named/unnamed consistency for insert_at.
# Runtime: O(1) from cached size/name counts.
.ft_assert_insert_name_state <- function(x, ins, context = "insert_at()") {
  m_x <- attr(x, "measures", exact = TRUE)
  m_i <- attr(ins, "measures", exact = TRUE)
  if(is.null(m_x) || is.null(m_i)) {
    stop("Tree has no measures attribute.")
  }
  n_x <- as.integer(m_x[[".size"]])
  nn_x <- as.integer(m_x[[".named_count"]])
  n_i <- as.integer(m_i[[".size"]])
  nn_i <- as.integer(m_i[[".named_count"]])

  if((n_x > 0L && nn_x != 0L && nn_x != n_x) || (n_i > 0L && nn_i != 0L && nn_i != n_i)) {
    stop("Invalid tree name state: mixed named/unnamed elements.")
  }
  if(n_x == 0L || n_i == 0L) {
    return(invisible(TRUE))
  }
  x_named <- (nn_x == n_x)
  i_named <- (nn_i == n_i)
  if(x_named != i_named) {
    stop("Cannot mix named and unnamed elements (", context, " would create mixed named and unnamed tree).")
  }
  invisible(TRUE)
}

#' Peek at the front element
#'
#' @param x A `flexseq`.
#' @return Front element.
#' @export
# Runtime: O(log n) lookup by scalar index.
peek_front <- function(x) {
  if(inherits(x, "interval_index")) {
    stop("`peek_front()` is not supported for interval_index. Use interval query helpers (`peek_*`, `pop_*`, and `peek_point()` for point lookup).")
  }
  if(inherits(x, "priority_queue")) {
    stop("`peek_front()` is not supported for priority_queue. Use `peek_min()`/`peek_max()`, or cast with `as_flexseq()`.")
  }
  if(!inherits(x, "flexseq")) {
    stop("`x` must be a flexseq.")
  }
  if(length(x) == 0L) {
    stop("Cannot `peek_front()` from an empty sequence.")
  }
  .ft_public_value(x, x[[1L]])
}

#' Peek at the back element
#'
#' @param x A `flexseq`.
#' @return Back element.
#' @export
# Runtime: O(log n) lookup by scalar index.
peek_back <- function(x) {
  if(inherits(x, "interval_index")) {
    stop("`peek_back()` is not supported for interval_index. Use interval query helpers (`peek_*`, `pop_*`, and `peek_point()` for point lookup).")
  }
  if(inherits(x, "priority_queue")) {
    stop("`peek_back()` is not supported for priority_queue. Use `peek_min()`/`peek_max()`, or cast with `as_flexseq()`.")
  }
  if(!inherits(x, "flexseq")) {
    stop("`x` must be a flexseq.")
  }
  n <- length(x)
  if(n == 0L) {
    stop("Cannot `peek_back()` from an empty sequence.")
  }
  .ft_public_value(x, x[[n]])
}

#' Peek at an element by position
#'
#' @param x A `flexseq`.
#' @param index One-based position to read.
#' @return Element at `index`.
#' @export
# Runtime: O(log n) lookup by scalar index.
peek_at <- function(x, index) {
  if(inherits(x, "interval_index")) {
    stop("`peek_at()` is not supported for interval_index. Use interval query helpers (`peek_*`, `pop_*`, and `peek_point()` for point lookup).")
  }
  if(inherits(x, "priority_queue")) {
    stop("`peek_at()` is not supported for priority_queue. Use `peek_min()`/`peek_max()`, or cast with `as_flexseq()`.")
  }
  if(!inherits(x, "flexseq")) {
    stop("`x` must be a flexseq.")
  }
  n <- length(x)
  if(n == 0L) {
    stop("Cannot `peek_at()` from an empty sequence.")
  }
  idx <- .ft_assert_one_positional_index(index, n)
  el <- .ft_strip_name(.ft_get_elem_at(x, idx))
  .ft_public_value(x, el)
}

#' Pop the front element
#'
#' Returns both the popped element and the remaining sequence.
#'
#' @param x A `flexseq`.
#' @return A list with fields `element` and `remaining`.
#' @examples
#' s <- as_flexseq(letters[1:3])
#' out <- pop_front(s)
#' out$element
#' out$remaining
#' @export
# Runtime: O(log n) for one read plus one subset.
pop_front <- function(x) {
  if(inherits(x, "interval_index")) {
    stop("`pop_front()` is not supported for interval_index. Use interval query helpers (`peek_*`, `pop_*`, and `peek_point()` for point lookup).")
  }
  if(inherits(x, "priority_queue")) {
    stop("`pop_front()` is not supported for priority_queue. Use `pop_min()`/`pop_max()`, or cast with `as_flexseq()`.")
  }
  if(!inherits(x, "flexseq")) {
    stop("`x` must be a flexseq.")
  }
  n <- length(x)
  if(n == 0L) {
    stop("Cannot `pop_front()` from an empty sequence.")
  }
  element <- .ft_public_value(x, x[[1L]])
  remaining <- if(n == 1L) .ft_empty_like(x, context = "pop_front()") else x[seq.int(2L, n)]
  list(element = element, remaining = remaining)
}

#' Pop the back element
#'
#' Returns both the popped element and the remaining sequence.
#'
#' @param x A `flexseq`.
#' @return A list with fields `element` and `remaining`.
#' @examples
#' s <- as_flexseq(letters[1:3])
#' out <- pop_back(s)
#' out$element
#' out$remaining
#' @export
# Runtime: O(log n) for one read plus one subset.
pop_back <- function(x) {
  if(inherits(x, "interval_index")) {
    stop("`pop_back()` is not supported for interval_index. Use interval query helpers (`peek_*`, `pop_*`, and `peek_point()` for point lookup).")
  }
  if(inherits(x, "priority_queue")) {
    stop("`pop_back()` is not supported for priority_queue. Use `pop_min()`/`pop_max()`, or cast with `as_flexseq()`.")
  }
  if(!inherits(x, "flexseq")) {
    stop("`x` must be a flexseq.")
  }
  n <- length(x)
  if(n == 0L) {
    stop("Cannot `pop_back()` from an empty sequence.")
  }
  element <- .ft_public_value(x, x[[n]])
  remaining <- if(n == 1L) .ft_empty_like(x, context = "pop_back()") else x[seq_len(n - 1L)]
  list(element = element, remaining = remaining)
}

#' Pop an element by position
#'
#' Returns both the popped element and the remaining sequence.
#'
#' @param x A `flexseq`.
#' @param index One-based position to remove.
#' @return A list with fields `element` and `remaining`.
#' @examples
#' s <- as_flexseq(letters[1:4])
#' out <- pop_at(s, 3)
#' out$element
#' out$remaining
#' @export
# Runtime: O(log n) for one read plus O(k log n) index subset to rebuild.
pop_at <- function(x, index) {
  if(inherits(x, "interval_index")) {
    stop("`pop_at()` is not supported for interval_index. Use interval query helpers (`peek_*`, `pop_*`, and `peek_point()` for point lookup).")
  }
  if(inherits(x, "priority_queue")) {
    stop("`pop_at()` is not supported for priority_queue. Use `pop_min()`/`pop_max()`, or cast with `as_flexseq()`.")
  }
  if(!inherits(x, "flexseq")) {
    stop("`x` must be a flexseq.")
  }
  n <- length(x)
  if(n == 0L) {
    stop("Cannot `pop_at()` from an empty sequence.")
  }
  idx <- .ft_assert_one_positional_index(index, n)
  el <- .ft_strip_name(.ft_get_elem_at(x, idx))
  element <- .ft_public_value(x, el)

  remaining <- if(n == 1L) {
    .ft_empty_like(x, context = "pop_at()")
  } else if(idx == 1L) {
    x[seq.int(2L, n)]
  } else if(idx == n) {
    x[seq_len(n - 1L)]
  } else {
    x[c(seq_len(idx - 1L), seq.int(idx + 1L, n))]
  }
  list(element = element, remaining = remaining)
}

#' Insert elements at a position
#'
#' Inserts one or more elements before the current element at `index`.
#'
#' @param x A `flexseq`.
#' @param index One-based insertion position in `[1, length(x) + 1]`.
#' @param values Elements to insert. Supports scalar/vector/list/flexseq inputs.
#' @return Updated sequence with inserted elements.
#' @examples
#' s <- as_flexseq(letters[1:4])
#' insert_at(s, 3, c("x", "y"))
#' @export
# Runtime: O(k log k) to build inserted payload + O(log n) split + concat work.
insert_at <- function(x, index, values) {
  if(inherits(x, "ordered_sequence")) {
    .ft_stop_ordered_like(x, "insert_at", "Use `insert()`.")
  }
  if(inherits(x, "priority_queue")) {
    stop("`insert_at()` is not supported for priority_queue. Cast first with `as_flexseq()`.")
  }
  if(!inherits(x, "flexseq")) {
    stop("`x` must be a flexseq.")
  }

  n <- length(x)
  idx <- .ft_assert_insert_index(index, n)
  vals <- .ft_insert_values_list(values)
  if(length(vals) == 0L) {
    return(x)
  }

  ms <- resolve_tree_monoids(x, required = TRUE)
  ins <- tree_from(vals, monoids = ms)
  .ft_assert_insert_name_state(x, ins, context = "insert_at()")

  out <- if(n == 0L) {
    ins
  } else if(idx == 1L) {
    concat_trees(ins, x)
  } else if(idx == (n + 1L)) {
    concat_trees(x, ins)
  } else {
    s <- split_by_predicate(x, function(v) v >= idx, ".size")
    concat_trees(concat_trees(s$left, ins), s$right)
  }
  .ft_restore_subclass(out, x, context = "insert_at()")
}
