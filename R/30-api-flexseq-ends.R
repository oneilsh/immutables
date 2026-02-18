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
    stop("`push_back()` is not supported for ordered_sequence/ordered_multiset. Use `insert()`.")
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
    stop("`push_front()` is not supported for ordered_sequence/ordered_multiset. Use `insert()`.")
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
  if(inherits(x, "ordered_sequence")) {
    return(.ord_wrap_like(x, e))
  }
  .ft_restore_subclass(e, x, context = context)
}

# Runtime: O(1).
.ft_public_value <- function(x, el) {
  if(inherits(x, "ordered_sequence")) {
    return(el$item)
  }
  el
}

#' Peek at the front element
#'
#' @param x A `flexseq`.
#' @return Front element.
#' @export
# Runtime: O(log n) lookup by scalar index.
peek_front <- function(x) {
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

#' Pop the front element
#'
#' Returns both the popped element and the remaining sequence.
#'
#' @param x A `flexseq`.
#' @return A list with fields `element` and `rest`.
#' @examples
#' s <- as_flexseq(letters[1:3])
#' out <- pop_front(s)
#' out$element
#' out$rest
#' @export
# Runtime: O(log n) for one read plus one subset.
pop_front <- function(x) {
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
  rest <- if(n == 1L) .ft_empty_like(x, context = "pop_front()") else x[seq.int(2L, n)]
  list(element = element, rest = rest)
}

#' Pop the back element
#'
#' Returns both the popped element and the remaining sequence.
#'
#' @param x A `flexseq`.
#' @return A list with fields `element` and `rest`.
#' @examples
#' s <- as_flexseq(letters[1:3])
#' out <- pop_back(s)
#' out$element
#' out$rest
#' @export
# Runtime: O(log n) for one read plus one subset.
pop_back <- function(x) {
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
  rest <- if(n == 1L) .ft_empty_like(x, context = "pop_back()") else x[seq_len(n - 1L)]
  list(element = element, rest = rest)
}
