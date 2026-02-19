# Runtime: O(1).
.oms_assert_set <- function(x) {
  if(!inherits(x, "ordered_sequence") || !is_structural_node(x)) {
    stop("`x` must be an ordered_sequence.")
  }
  invisible(TRUE)
}

# Runtime: O(1).
.oms_key_type_state <- function(x) {
  attr(x, "oms_key_type", exact = TRUE)
}

# Runtime: O(1).
.oms_make_entry <- function(item, key_value) {
  list(item = item, key = key_value)
}

# Runtime: O(n log n) from ordering by key then stable position.
.oms_order_entries <- function(entries, key_type) {
  if(length(entries) == 0L) {
    return(entries)
  }
  idx <- seq_along(entries)
  ord <- if(key_type == "numeric") {
    order(vapply(entries, function(e) e$key, numeric(1)), idx)
  } else if(key_type == "character") {
    order(vapply(entries, function(e) e$key, character(1)), idx)
  } else {
    order(vapply(entries, function(e) as.integer(isTRUE(e$key)), integer(1)), idx)
  }
  entries[ord]
}

# Runtime: O(n) in entry count.
.oms_prepare_entry_names <- function(entries) {
  if(length(entries) == 0L) {
    return(entries)
  }
  nms <- names(entries)
  if(is.null(nms)) {
    return(entries)
  }

  out <- entries
  for(i in seq_along(out)) {
    nm <- .ft_normalize_name(nms[[i]])
    if(!is.null(nm)) {
      out[[i]] <- .ft_set_name(out[[i]], nm)
    }
  }
  names(out) <- NULL
  out
}

# Runtime: O(1).
.as_ordered_sequence <- function(x, key_type) {
  if(!is_structural_node(x)) {
    stop("Expected a structural tree node.")
  }
  class(x) <- unique(c("ordered_sequence", "flexseq", setdiff(class(x), "list")))
  attr(x, "oms_key_type") <- key_type
  x
}

# Runtime: O(1).
.ord_wrap_like <- function(template, tree, key_type = NULL) {
  resolved_key_type <- if(is.null(key_type)) .oms_key_type_state(template) else key_type
  .as_ordered_sequence(tree, key_type = resolved_key_type)
}

# Runtime: O(1).
.oms_wrap_tree <- function(template, tree, key_type = NULL) {
  .ord_wrap_like(template, tree, key_type = key_type)
}

# Runtime: O(1).
.oms_validate_key_type <- function(oms_key_type, new_key_type) {
  if(is.null(oms_key_type)) {
    return(new_key_type)
  }
  if(!identical(oms_key_type, new_key_type)) {
    stop("Incompatible key type for this ordered_sequence.")
  }
  oms_key_type
}

# Runtime: O(1).
.oms_as_key_list <- function(keys, n) {
  key_list <- as.list(keys)
  if(length(key_list) != n) {
    stop("`keys` length must match elements length.")
  }
  key_list
}

# Runtime: O(n log n) for sorting + O(n) bulk build.
.oms_build_from_items <- function(items, keys = NULL, monoids = NULL) {
  n <- length(items)

  if(n == 0L) {
    if(!is.null(keys) && length(as.list(keys)) > 0L) {
      stop("`keys` must be empty when no elements are supplied.")
    }
    base <- as_flexseq(list(), monoids = .oms_merge_monoids(monoids))
    return(.as_ordered_sequence(base, key_type = NULL))
  }

  if(is.null(keys)) {
    stop("`keys` is required when elements are supplied.")
  }
  keys <- .oms_as_key_list(keys, n)

  entries <- vector("list", n)
  item_names <- names(items)
  key_type <- NULL
  for(i in seq_len(n)) {
    norm <- .oms_normalize_key(keys[[i]])
    key_type <- .oms_validate_key_type(key_type, norm$key_type)
    entries[[i]] <- .oms_make_entry(items[[i]], norm$key)
  }
  if(!is.null(item_names) && length(item_names) == n) {
    names(entries) <- item_names
  }

  entries <- .oms_order_entries(entries, key_type)

  merged_monoids <- .oms_merge_monoids(monoids)
  base <- .oms_tree_from_ordered_entries(entries, merged_monoids)
  .as_ordered_sequence(base, key_type = key_type)
}

# Runtime: O(1).
.oms_empty_tree_like <- function(template) {
  ms <- attr(template, "monoids", exact = TRUE)
  .as_flexseq(measured_empty(ms))
}

# Runtime: O(n) for ordered entries.
.oms_tree_from_ordered_entries <- function(entries, monoids) {
  entries <- .oms_prepare_entry_names(entries)
  if(.ft_cpp_can_use(monoids)) {
    return(.as_flexseq(.ft_cpp_tree_from_sorted(entries, monoids)))
  }
  .ft_tree_from_list_linear(entries, monoids)
}

# Runtime: O(log n) near locate point depth.
.oms_bound_index_prepared <- function(x, key, strict = FALSE) {
  .oms_assert_set(x)
  n <- length(x)
  if(n == 0L) {
    return(1L)
  }

  pred <- if(!isTRUE(strict)) {
    function(v) {
      isTRUE(v$has) && .oms_compare_key(v$key, key, v$key_type) >= 0L
    }
  } else {
    function(v) {
      isTRUE(v$has) && .oms_compare_key(v$key, key, v$key_type) > 0L
    }
  }

  loc <- locate_by_predicate(x, pred, ".oms_max_key", include_metadata = TRUE)
  if(!isTRUE(loc$found)) {
    return(as.integer(n + 1L))
  }
  as.integer(loc$metadata$index)
}

# Runtime: O(log n) near locate point depth.
.oms_bound_index <- function(x, key_value, strict = FALSE) {
  .oms_assert_set(x)
  key_type <- .oms_key_type_state(x)
  norm <- .oms_normalize_key(key_value)
  .oms_validate_key_type(key_type, norm$key_type)
  .oms_bound_index_prepared(x, norm$key, strict = strict)
}

# Runtime: O(1).
.oms_range_start_index <- function(x, lo, include_lo) {
  if(isTRUE(include_lo)) {
    .oms_bound_index(x, lo, strict = FALSE)
  } else {
    .oms_bound_index(x, lo, strict = TRUE)
  }
}

# Runtime: O(1).
.oms_range_end_exclusive_index <- function(x, hi, include_hi) {
  if(isTRUE(include_hi)) {
    .oms_bound_index(x, hi, strict = TRUE)
  } else {
    .oms_bound_index(x, hi, strict = FALSE)
  }
}

# Runtime: O(1).
.oms_coerce_lgl_scalar <- function(v, arg_name) {
  if(!is.logical(v) || length(v) != 1L || is.na(v)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", arg_name))
  }
  isTRUE(v)
}

# Runtime: O(n) in number of selected entries.
.oms_extract_items <- function(entries) {
  lapply(entries, function(e) e$item)
}

# Runtime: O(n).
.oms_entries <- function(x) {
  as.list.flexseq(x)
}

# Runtime: O(log n) via indexed locate path.
.oms_entry_at <- function(x, idx) {
  .ft_get_elem_at(x, as.integer(idx))
}

# Runtime: O(n log n) from build and ordering.
#' Build an Ordered Sequence from elements
#'
#' @param x Elements to add.
#' @param keys Scalar key values matching `x` length.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#'
#' Ordered sequences are always key-sorted. Subsetting with `[` is intentionally
#' constrained to strictly increasing mapped positions (no duplicates or
#' reordering) so the result remains ordered.
#' @return An `ordered_sequence`.
#' @examples
#' xs <- as_ordered_sequence(c(4, 1, 2, 1), keys = c(4, 1, 2, 1))
#' xs
#' length(elements_between(xs, 1, 1))
#' @export
as_ordered_sequence <- function(x, keys = NULL, monoids = NULL) {
  .oms_build_from_items(as.list(x), keys = keys, monoids = monoids)
}

# Runtime: O(n log n) from build and ordering.
#' Construct an Ordered Sequence
#'
#' @param ... Elements to add.
#' @param keys Scalar key values matching `...` length.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#' @return An `ordered_sequence`.
#' @examples
#' xs <- ordered_sequence("bb", "a", "ccc", keys = c(2, 1, 3))
#' xs
#' lower_bound(xs, 2)
#' @export
ordered_sequence <- function(..., keys = NULL, monoids = NULL) {
  as_ordered_sequence(list(...), keys = keys, monoids = monoids)
}

# Runtime: O(log n) near insertion/split point depth.
#' @noRd
.oms_insert_impl <- function(x, element, key) {
  .oms_assert_set(x)
  norm <- .oms_normalize_key(key)
  key_type <- .oms_validate_key_type(.oms_key_type_state(x), norm$key_type)

  entry <- .oms_make_entry(element, norm$key)
  ms <- attr(x, "monoids", exact = TRUE)

  out <- if(.ft_cpp_can_use(ms)) {
    .as_flexseq(.ft_cpp_oms_insert(x, entry, ms, key_type))
  } else {
    s <- split_by_predicate(
      x,
      function(v) isTRUE(v$has) && .oms_compare_key(v$key, norm$key, v$key_type) > 0L,
      ".oms_max_key"
    )
    # Internal append avoids public ordered_sequence push guards on boundary splits.
    left_plus <- .ft_push_back_impl(s$left, entry, context = "insert()")
    concat_trees(left_plus, s$right)
  }

  .ord_wrap_like(x, out, key_type = key_type)
}

# Runtime: O(log n) near insertion/split point depth.
#' @rdname insert
#' @param key Scalar key for `element`.
#' @return Updated `ordered_sequence`.
#' @export
insert.ordered_sequence <- function(x, element, key, ...) {
  .oms_insert_impl(x, element, key)
}

# Runtime: O(log n).
.oms_key_span <- function(x, key) {
  .oms_assert_set(x)
  norm <- .oms_normalize_key(key)
  .oms_validate_key_type(.oms_key_type_state(x), norm$key_type)

  start <- .oms_bound_index_prepared(x, norm$key, strict = FALSE)
  end_excl <- .oms_bound_index_prepared(x, norm$key, strict = TRUE)
  list(
    found = isTRUE(end_excl > start),
    key = norm$key,
    start = as.integer(start),
    end_excl = as.integer(end_excl)
  )
}

# Runtime: O(log n) near split points.
.oms_slice_key_span <- function(x, start, end_excl) {
  .oms_assert_set(x)
  if(end_excl <= start) {
    empty <- .oms_empty_tree_like(x)
    return(list(matched = empty, rest = .as_flexseq(x)))
  }
  s1 <- split_by_predicate(x, function(v) v >= start, ".size")
  span_len <- as.integer(end_excl - start)
  s2 <- split_by_predicate(s1$right, function(v) v >= (span_len + 1L), ".size")
  list(
    matched = s2$left,
    rest = concat_trees(s1$left, s2$right)
  )
}

# Runtime: O(log n).
#' Find first element with key >= value
#'
#' @param x An `ordered_sequence`.
#' @param key Query key.
#' @return Named list with fields `found`, `index`, `element`, and `key`.
#'   When no match exists, `found` is `FALSE` and the remaining fields
#'   are `NULL`.
#' @export
lower_bound <- function(x, key) {
  .oms_assert_set(x)
  idx <- .oms_bound_index(x, key, strict = FALSE)
  n <- length(x)

  if(idx > n) {
    return(list(found = FALSE, index = NULL, element = NULL, key = NULL))
  }

  entry <- .oms_entry_at(x, idx)
  list(found = TRUE, index = idx, element = entry$item, key = entry$key)
}

# Runtime: O(log n).
#' Find first element with key > value
#'
#' @param x An `ordered_sequence`.
#' @param key Query key.
#' @return Named list with fields `found`, `index`, `element`, and `key`.
#'   When no match exists, `found` is `FALSE` and the remaining fields
#'   are `NULL`.
#' @export
upper_bound <- function(x, key) {
  .oms_assert_set(x)
  idx <- .oms_bound_index(x, key, strict = TRUE)
  n <- length(x)

  if(idx > n) {
    return(list(found = FALSE, index = NULL, element = NULL, key = NULL))
  }

  entry <- .oms_entry_at(x, idx)
  list(found = TRUE, index = idx, element = entry$item, key = entry$key)
}

# Runtime: O(log n) for `which = "first"`; O(log n) for `which = "all"`.
#' Peek elements for one key
#'
#' For `which = "first"`, returns the first (leftmost) sequence element among
#' entries whose key equals `key`.
#'
#' For `which = "all"`, returns an ordered sequence containing exactly the
#' entries whose key equals `key`.
#'
#' @param x An `ordered_sequence`.
#' @param key Query key.
#' @param which One of `"first"` or `"all"`.
#' @return For `which = "first"`, raw stored element.
#'   For `which = "all"`, an ordered sequence with matching elements.
#'   Throws when no matching key exists.
#' @export
peek_key <- function(x, key, which = c("first", "all")) {
  which <- match.arg(which)
  span <- .oms_key_span(x, key)

  if(!isTRUE(span$found)) {
    stop("No matching key found.")
  }

  if(!identical(which, "all")) {
    s <- split_around_by_predicate(x, function(v) v >= span$start, ".size")
    return(s$elem$item)
  }

  parts <- .oms_slice_key_span(x, span$start, span$end_excl)
  .ord_wrap_like(x, parts$matched)
}

# Runtime: O(log n) near split point depth.
#' Pop elements for one key
#'
#' For `which = "first"`, removes and returns the first (leftmost) sequence
#' element among entries whose key equals `key`.
#'
#' For `which = "all"`, removes and returns all matching entries as an ordered
#' sequence.
#'
#' @param x An `ordered_sequence`.
#' @param key Query key.
#' @param which One of `"first"` or `"all"`.
#' @return A named list with components \code{element}, \code{key}, and
#'   \code{remaining}.
#'   \itemize{
#'   \item For \code{which = "first"}:
#'   \itemize{
#'   \item On match: \code{element} is the first matching item and \code{key}
#'   is its key.
#'   \item On miss: \code{element = NULL}, \code{key = NULL},
#'   \code{remaining = x}.
#'   }
#'   \item For \code{which = "all"}:
#'   \itemize{
#'   \item \code{element} is an \code{ordered_sequence} of all matching items in
#'   stable order. It may have size 0 (miss), 1 (single match), or greater than
#'   1 (multiple matches).
#'   \item \code{key} is the normalized key on match, otherwise \code{NULL}.
#'   \item \code{remaining} is the original sequence with that full key-run
#'   removed (or unchanged on miss).
#'   }
#'   }
#' @export
pop_key <- function(x, key, which = c("first", "all")) {
  which <- match.arg(which)
  span <- .oms_key_span(x, key)

  if(!isTRUE(span$found)) {
    if(identical(which, "all")) {
      return(list(element = .ord_wrap_like(x, .oms_empty_tree_like(x)), key = NULL, remaining = x))
    }
    return(list(element = NULL, key = NULL, remaining = x))
  }

  if(!identical(which, "all")) {
    s <- split_around_by_predicate(x, function(v) v >= span$start, ".size")
    out <- concat_trees(s$left, s$right)
    seq_out <- .ord_wrap_like(x, out)
    return(list(element = s$elem$item, key = s$elem$key, remaining = seq_out))
  }
  parts <- .oms_slice_key_span(x, span$start, span$end_excl)
  list(
    element = .ord_wrap_like(x, parts$matched),
    key = span$key,
    remaining = .ord_wrap_like(x, parts$rest)
  )
}

# Runtime: O(n log n) total from traversal + reordering + rebuild.
.oms_apply_impl <- function(x, f, ...) {
  .oms_assert_set(x)
  if(!is.function(f)) {
    stop("`FUN` must be a function.")
  }

  entries <- .oms_entries(x)
  n <- length(entries)
  if(n == 0L) {
    return(x)
  }

  out <- vector("list", n)
  out_names <- if(is.null(names(entries))) rep("", n) else names(entries)
  key_type <- .oms_key_type_state(x)

  for(i in seq_len(n)) {
    e <- entries[[i]]
    cur_name <- out_names[[i]]

    upd <- f(e$item, e$key, cur_name, ...)
    if(!is.list(upd)) {
      stop("`FUN` must return a list.")
    }
    if(length(upd) > 0L) {
      nm <- names(upd)
      if(is.null(nm) || any(is.na(nm)) || any(nm == "")) {
        stop("`FUN` must return a named list using only: item, key, name.")
      }
      if(anyDuplicated(nm) > 0L) {
        stop("`FUN` return list cannot contain duplicated field names.")
      }
      bad <- setdiff(nm, c("item", "key", "name"))
      if(length(bad) > 0L) {
        stop("`FUN` returned unsupported field(s): ", paste(bad, collapse = ", "))
      }
    }

    item2 <- if("item" %in% names(upd)) upd[["item"]] else e$item
    key2 <- e$key
    if("key" %in% names(upd)) {
      norm <- .oms_normalize_key(upd[["key"]])
      key_type <- .oms_validate_key_type(key_type, norm$key_type)
      key2 <- norm$key
    }
    if("name" %in% names(upd)) {
      nm2 <- .ft_normalize_name(upd[["name"]])
      out_names[[i]] <- if(is.null(nm2)) "" else nm2
    }

    out[[i]] <- .oms_make_entry(item = item2, key_value = key2)
  }

  if(any(out_names != "")) {
    names(out) <- out_names
  }

  out <- .oms_order_entries(out, key_type)
  ms <- attr(x, "monoids", exact = TRUE)
  out_tree <- .oms_tree_from_ordered_entries(out, ms)
  .ord_wrap_like(x, out_tree, key_type = key_type)
}

#' Lapply over ordered sequence entries
#'
#' @rdname lapply
#' @method lapply ordered_sequence
#' @export
lapply.ordered_sequence <- function(X, FUN, ...) {
  if(!is.function(FUN)) {
    stop("`FUN` must be a function.")
  }
  .oms_apply_impl(X, FUN, ...)
}

# Runtime: O(log n + k), where k is output size.
#' Return elements in a key range
#'
#' @param x An `ordered_sequence`.
#' @param from_key Lower bound key.
#' @param to_key Upper bound key.
#' @param include_from Include lower bound when `TRUE`.
#' @param include_to Include upper bound when `TRUE`.
#' @return List of raw elements.
#' @export
elements_between <- function(x, from_key, to_key, include_from = TRUE, include_to = TRUE) {
  .oms_assert_set(x)
  include_from <- .oms_coerce_lgl_scalar(include_from, "include_from")
  include_to <- .oms_coerce_lgl_scalar(include_to, "include_to")

  start <- .oms_range_start_index(x, from_key, include_from)
  end_excl <- .oms_range_end_exclusive_index(x, to_key, include_to)

  if(end_excl <= start || start > length(x)) {
    return(list())
  }

  entries <- .ft_get_elems_at(x, seq.int(start, end_excl - 1L))
  .oms_extract_items(entries)
}

# Runtime: O(log n).
#' Count elements matching one key
#'
#' @param x An `ordered_sequence`.
#' @param key Query key.
#' @return Integer count of matching elements.
#' @export
count_key <- function(x, key) {
  .oms_assert_set(x)
  lo <- .oms_bound_index(x, key, strict = FALSE)
  hi <- .oms_bound_index(x, key, strict = TRUE)
  as.integer(max(0L, hi - lo))
}

# Runtime: O(log n).
#' Count elements in a key range
#'
#' @param x An `ordered_sequence`.
#' @param from_key Lower bound key.
#' @param to_key Upper bound key.
#' @param include_from Include lower bound when `TRUE`.
#' @param include_to Include upper bound when `TRUE`.
#' @return Integer count of matching elements.
#' @export
count_between <- function(x, from_key, to_key, include_from = TRUE, include_to = TRUE) {
  .oms_assert_set(x)
  include_from <- .oms_coerce_lgl_scalar(include_from, "include_from")
  include_to <- .oms_coerce_lgl_scalar(include_to, "include_to")

  start <- .oms_range_start_index(x, from_key, include_from)
  end_excl <- .oms_range_end_exclusive_index(x, to_key, include_to)
  as.integer(max(0L, end_excl - start))
}

#' @method as.list ordered_sequence
#' @export
# Runtime: O(n).
as.list.ordered_sequence <- function(x, ...) {
  .oms_assert_set(x)
  .oms_extract_items(as.list.flexseq(x, ...))
}

#' @method length ordered_sequence
#' @export
# Runtime: O(1).
length.ordered_sequence <- function(x) {
  .oms_assert_set(x)
  as.integer(node_measure(x, ".size"))
}
