# Runtime: O(1).
.ivx_assert_index <- function(x) {
  if(!inherits(x, "interval_index") || !is_structural_node(x)) {
    stop("`x` must be an interval_index.")
  }
  invisible(TRUE)
}

# Runtime: O(1).
.ivx_endpoint_type_state <- function(x) {
  attr(x, "ivx_endpoint_type", exact = TRUE)
}

# Runtime: O(1).
.ivx_bounds_state <- function(x) {
  b <- attr(x, "ivx_bounds", exact = TRUE)
  if(is.null(b)) "[)" else b
}

# Runtime: O(1).
.ivx_normalize_bounds <- function(bounds) {
  if(!is.character(bounds) || length(bounds) != 1L || is.na(bounds)) {
    stop("`bounds` must be one of: [), [], (), (].")
  }
  if(!(bounds %in% c("[)", "[]", "()", "(]"))) {
    stop("`bounds` must be one of: [), [], (), (].")
  }
  bounds
}

# Runtime: O(1).
.ivx_bounds_flags <- function(bounds) {
  list(
    include_start = identical(substr(bounds, 1L, 1L), "["),
    include_end = identical(substr(bounds, 2L, 2L), "]")
  )
}

# Runtime: O(1).
.ivx_normalize_endpoint <- function(value, arg_name, endpoint_type = NULL) {
  if(length(value) != 1L) {
    stop(sprintf("`%s` must be a scalar value.", arg_name))
  }

  na_flag <- suppressWarnings(tryCatch(is.na(value), error = function(e) FALSE))
  if(is.logical(na_flag) && length(na_flag) == 1L && isTRUE(na_flag)) {
    stop(sprintf("`%s` must be non-missing.", arg_name))
  }

  vt <- .ivx_endpoint_type(value)
  if(!is.null(endpoint_type) && !identical(endpoint_type, vt)) {
    stop("Incompatible endpoint type for this interval_index.")
  }

  # Validate orderability for endpoint objects using base Ops.
  .ivx_compare_scalar(value, value, vt)

  list(value = value, endpoint_type = vt)
}

# Runtime: O(1).
.ivx_normalize_interval <- function(start, end, endpoint_type = NULL, start_name = "start", end_name = "end") {
  ns <- .ivx_normalize_endpoint(start, start_name, endpoint_type = endpoint_type)
  ne <- .ivx_normalize_endpoint(end, end_name, endpoint_type = ns$endpoint_type)

  if(.ivx_compare_scalar(ns$value, ne$value, ns$endpoint_type) > 0L) {
    stop("`start` must be <= `end`.")
  }

  list(start = ns$value, end = ne$value, endpoint_type = ns$endpoint_type)
}

# Runtime: O(1).
.ivx_make_entry <- function(item, start, end) {
  list(item = item, start = start, end = end, key = start)
}

# Runtime: O(1).
.as_interval_index <- function(x, endpoint_type = NULL, bounds = "[)") {
  if(!is_structural_node(x)) {
    stop("Expected a structural tree node.")
  }

  class(x) <- unique(c("interval_index", "ordered_sequence", "flexseq", setdiff(class(x), "list")))
  attr(x, "ivx_endpoint_type") <- endpoint_type
  attr(x, "ivx_bounds") <- .ivx_normalize_bounds(bounds)

  # Keep ordered_sequence key type available only for primitive key classes.
  attr(x, "oms_key_type") <- if(is.null(endpoint_type) || !(endpoint_type %in% c("numeric", "character", "logical"))) {
    NULL
  } else {
    endpoint_type
  }

  x
}

# Runtime: O(1).
.ivx_wrap_like <- function(template, tree, endpoint_type = NULL, bounds = NULL) {
  ep <- if(is.null(endpoint_type)) .ivx_endpoint_type_state(template) else endpoint_type
  b <- if(is.null(bounds)) .ivx_bounds_state(template) else bounds
  .as_interval_index(tree, endpoint_type = ep, bounds = b)
}

# Runtime: O(1).
.ivx_parse_entry <- function(entry, context = "interval_index", endpoint_type = NULL) {
  if(!is.list(entry)) {
    stop(context, " entries must be named lists with fields: item, start, end (optional: key).")
  }

  nm <- names(entry)
  if(is.null(nm) || any(is.na(nm)) || any(nm == "")) {
    stop(context, " entries must be named lists with fields: item, start, end (optional: key).")
  }
  if(anyDuplicated(nm) > 0L) {
    stop(context, " entry fields must be unique.")
  }

  bad <- setdiff(nm, c("item", "start", "end", "key"))
  if(length(bad) > 0L) {
    stop(context, " entry contains unsupported field(s): ", paste(bad, collapse = ", "))
  }
  if(!("item" %in% nm) || !("start" %in% nm) || !("end" %in% nm)) {
    stop(context, " entries must include `item`, `start`, and `end`.")
  }

  norm <- .ivx_normalize_interval(entry[["start"]], entry[["end"]], endpoint_type = endpoint_type)

  if("key" %in% nm) {
    if(.ivx_compare_scalar(entry[["key"]], norm$start, norm$endpoint_type) != 0L) {
      stop(context, " entry `key` must equal `start`.")
    }
  }

  list(
    entry = .ivx_make_entry(entry[["item"]], norm$start, norm$end),
    endpoint_type = norm$endpoint_type
  )
}

# Runtime: O(n) to validate all entries.
.ivx_validate_tree_entries <- function(x, endpoint_type = NULL, context = "interval_index") {
  els <- .ft_to_list(x)
  if(length(els) == 0L) {
    return(endpoint_type)
  }

  out_type <- endpoint_type
  for(el in els) {
    parsed <- .ivx_parse_entry(el, context = context, endpoint_type = out_type)
    out_type <- parsed$endpoint_type
  }

  out_type
}

# Runtime: O(n) validation + O(1) wrap.
.ivx_restore_tree <- function(x, template = NULL, context = "interval_index") {
  endpoint_type <- if(is.null(template)) NULL else .ivx_endpoint_type_state(template)
  bounds <- if(is.null(template)) "[)" else .ivx_bounds_state(template)
  endpoint_type <- .ivx_validate_tree_entries(x, endpoint_type = endpoint_type, context = context)
  .as_interval_index(x, endpoint_type = endpoint_type, bounds = bounds)
}

# Runtime: O(n).
.ivx_entries <- function(x) {
  as.list.flexseq(x)
}

# Runtime: O(n).
.ivx_extract_items <- function(entries) {
  out <- lapply(entries, function(e) e$item)
  names(out) <- names(entries)
  out
}

# Runtime: O(m), where m = number of attached monoids.
.ivx_user_monoids <- function(x) {
  ms <- attr(x, "monoids", exact = TRUE)
  out <- ms[setdiff(names(ms), c(".size", ".named_count", ".ivx_max_start"))]
  if(length(out) == 0L) {
    return(NULL)
  }
  out
}

# Runtime: O(1).
.ivx_empty_like <- function(template) {
  ms <- resolve_tree_monoids(template, required = TRUE)
  .ivx_wrap_like(template, empty_tree(monoids = ms))
}

# Runtime: O(log n) near insertion/split point depth.
.ivx_insert_entry <- function(x, entry, endpoint_type = NULL) {
  .ivx_assert_index(x)

  ep <- if(is.null(endpoint_type)) .ivx_endpoint_type_state(x) else endpoint_type
  out <- if(length(x) == 0L) {
    .ft_push_back_impl(x, entry, context = "insert()")
  } else {
    s <- split_by_predicate(
      x,
      function(v) isTRUE(v$has) && .ivx_compare_scalar(v$start, entry$start, v$endpoint_type) > 0L,
      ".ivx_max_start"
    )
    left_plus <- .ft_push_back_impl(s$left, entry, context = "insert()")
    concat_trees(left_plus, s$right)
  }

  .ivx_wrap_like(x, out, endpoint_type = ep)
}

# Runtime: O(n) in entry count.
.ivx_prepare_entry_names <- function(entries) {
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

# Runtime: O(n) for ordered entries.
.ivx_tree_from_ordered_entries <- function(entries, monoids) {
  entries <- .ivx_prepare_entry_names(entries)
  if(.ft_cpp_can_use(monoids)) {
    return(.as_flexseq(.ft_cpp_tree_from_sorted(entries, monoids)))
  }
  .ft_tree_from_list_linear(entries, monoids)
}

# Runtime: O(n log n) for stable merge sort by start.
.ivx_merge_sort_indices <- function(idx, entries, endpoint_type) {
  n <- length(idx)
  if(n <= 1L) {
    return(idx)
  }

  mid <- as.integer(n %/% 2L)
  left <- .ivx_merge_sort_indices(idx[seq_len(mid)], entries, endpoint_type)
  right <- .ivx_merge_sort_indices(idx[(mid + 1L):n], entries, endpoint_type)

  out <- integer(n)
  i <- 1L
  j <- 1L
  k <- 1L
  while(i <= length(left) && j <= length(right)) {
    cmp <- .ivx_compare_scalar(entries[[left[[i]]]]$start, entries[[right[[j]]]]$start, endpoint_type)
    if(cmp <= 0L) {
      out[[k]] <- left[[i]]
      i <- i + 1L
    } else {
      out[[k]] <- right[[j]]
      j <- j + 1L
    }
    k <- k + 1L
  }

  while(i <= length(left)) {
    out[[k]] <- left[[i]]
    i <- i + 1L
    k <- k + 1L
  }
  while(j <= length(right)) {
    out[[k]] <- right[[j]]
    j <- j + 1L
    k <- k + 1L
  }
  out
}

# Runtime: O(n log n) stable by start and FIFO on ties.
.ivx_order_entries <- function(entries, endpoint_type) {
  if(length(entries) <= 1L) {
    return(entries)
  }

  idx <- seq_along(entries)
  starts <- lapply(entries, function(e) e$start)
  ord <- tryCatch(
    order(do.call(c, starts), idx),
    error = function(e) NULL
  )
  if(is.null(ord) || length(ord) != length(entries)) {
    ord <- .ivx_merge_sort_indices(idx, entries, endpoint_type)
  }
  entries[ord]
}

# Runtime: O(n log n) from sort + bulk build.
.ivx_build_from_items <- function(items, start = NULL, end = NULL, bounds = "[)", monoids = NULL) {
  n <- length(items)

  if(n == 0L) {
    if(!is.null(start) && length(as.list(start)) > 0L) {
      stop("`start` must be empty when no elements are supplied.")
    }
    if(!is.null(end) && length(as.list(end)) > 0L) {
      stop("`end` must be empty when no elements are supplied.")
    }

    base <- as_flexseq(list(), monoids = .ivx_merge_monoids(monoids))
    return(.as_interval_index(base, endpoint_type = NULL, bounds = bounds))
  }

  if(is.null(start)) {
    stop("`start` is required when elements are supplied.")
  }
  if(is.null(end)) {
    stop("`end` is required when elements are supplied.")
  }

  starts <- as.list(start)
  ends <- as.list(end)

  if(length(starts) != n) {
    stop("`start` length must match elements length.")
  }
  if(length(ends) != n) {
    stop("`end` length must match elements length.")
  }

  entries <- vector("list", n)
  item_names <- names(items)
  endpoint_type <- NULL

  for(i in seq_len(n)) {
    norm <- .ivx_normalize_interval(starts[[i]], ends[[i]], endpoint_type = endpoint_type)
    endpoint_type <- norm$endpoint_type

    entries[[i]] <- .ivx_make_entry(items[[i]], norm$start, norm$end)
  }
  if(!is.null(item_names) && length(item_names) == n) {
    names(entries) <- item_names
  }

  entries <- .ivx_order_entries(entries, endpoint_type)
  merged_monoids <- .ivx_merge_monoids(monoids)
  base <- .ivx_tree_from_ordered_entries(entries, merged_monoids)
  .as_interval_index(base, endpoint_type = endpoint_type, bounds = bounds)
}

# Runtime: O(1).
.ivx_resolve_bounds <- function(x, bounds = NULL) {
  if(is.null(bounds)) {
    return(.ivx_bounds_state(x))
  }
  .ivx_normalize_bounds(bounds)
}

# Runtime: O(1).
.ivx_contains_point <- function(start, end, point, bounds, endpoint_type) {
  f <- .ivx_bounds_flags(bounds)
  cmp_lo <- .ivx_compare_scalar(point, start, endpoint_type)
  cmp_hi <- .ivx_compare_scalar(point, end, endpoint_type)

  left_ok <- if(isTRUE(f$include_start)) cmp_lo >= 0L else cmp_lo > 0L
  right_ok <- if(isTRUE(f$include_end)) cmp_hi <= 0L else cmp_hi < 0L
  isTRUE(left_ok && right_ok)
}

# Runtime: O(1).
.ivx_overlaps_interval <- function(a_start, a_end, b_start, b_end, bounds, endpoint_type) {
  f <- .ivx_bounds_flags(bounds)
  touching_is_overlap <- isTRUE(f$include_start) && isTRUE(f$include_end)

  cmp_aend_bstart <- .ivx_compare_scalar(a_end, b_start, endpoint_type)
  cmp_bend_astart <- .ivx_compare_scalar(b_end, a_start, endpoint_type)

  a_before_b <- if(touching_is_overlap) cmp_aend_bstart < 0L else cmp_aend_bstart <= 0L
  b_before_a <- if(touching_is_overlap) cmp_bend_astart < 0L else cmp_bend_astart <= 0L

  !isTRUE(a_before_b || b_before_a)
}

# Runtime: O(1).
.ivx_contains_interval <- function(container_start, container_end, inner_start, inner_end, endpoint_type) {
  left_ok <- .ivx_compare_scalar(container_start, inner_start, endpoint_type) <= 0L
  right_ok <- .ivx_compare_scalar(container_end, inner_end, endpoint_type) >= 0L
  isTRUE(left_ok && right_ok)
}

# Runtime: O(n).
.ivx_match_positions <- function(x, predicate) {
  entries <- .ivx_entries(x)
  n <- length(entries)
  if(n == 0L) {
    return(integer(0))
  }

  out <- logical(n)
  for(i in seq_len(n)) {
    out[[i]] <- isTRUE(predicate(entries[[i]]))
  }
  which(out)
}

# Runtime: O(k) in matched entry count.
.ivx_slice_entries <- function(x, entries) {
  if(length(entries) == 0L) {
    return(.ivx_empty_like(x))
  }

  ms <- resolve_tree_monoids(x, required = TRUE)
  tree <- .ivx_tree_from_ordered_entries(entries, monoids = ms)
  .ivx_wrap_like(x, tree)
}

# Runtime: O(c + k), where c = candidate count and k = matched count.
.ivx_filter_slice <- function(x, entries, predicate) {
  n <- length(entries)
  if(n == 0L) {
    return(.ivx_empty_like(x))
  }

  keep <- logical(n)
  for(i in seq_len(n)) {
    keep[[i]] <- isTRUE(predicate(entries[[i]]))
  }
  .ivx_slice_entries(x, entries[keep])
}

# Runtime: O(log n) near split points.
.ivx_slice_start_range <- function(x, lower = NULL, lower_strict = FALSE, upper = NULL, upper_strict = FALSE) {
  out <- x

  if(!is.null(lower)) {
    pred_lo <- if(isTRUE(lower_strict)) {
      function(v) {
        isTRUE(v$has) && .ivx_compare_scalar_fast(v$start, lower, v$endpoint_type) > 0L
      }
    } else {
      function(v) {
        isTRUE(v$has) && .ivx_compare_scalar_fast(v$start, lower, v$endpoint_type) >= 0L
      }
    }
    out <- split_by_predicate(out, pred_lo, ".ivx_max_start")$right
  }

  if(!is.null(upper)) {
    pred_hi <- if(isTRUE(upper_strict)) {
      function(v) {
        isTRUE(v$has) && .ivx_compare_scalar_fast(v$start, upper, v$endpoint_type) >= 0L
      }
    } else {
      function(v) {
        isTRUE(v$has) && .ivx_compare_scalar_fast(v$start, upper, v$endpoint_type) > 0L
      }
    }
    out <- split_by_predicate(out, pred_hi, ".ivx_max_start")$left
  }

  out
}

# Runtime: O(log n + c), where c = candidate count after start-range pruning.
.ivx_query_candidate_entries <- function(x, lower = NULL, lower_strict = FALSE, upper = NULL, upper_strict = FALSE) {
  .ivx_entries(.ivx_slice_start_range(
    x,
    lower = lower,
    lower_strict = lower_strict,
    upper = upper,
    upper_strict = upper_strict
  ))
}

# Runtime: O(k log n), where k = length(positions).
.ivx_slice_positions <- function(x, positions) {
  if(length(positions) == 0L) {
    return(.ivx_empty_like(x))
  }
  x[as.integer(positions)]
}

# Runtime: O(n log n) from indexed subset rebuild.
.ivx_remove_positions <- function(x, positions) {
  n <- length(x)
  if(length(positions) == 0L) {
    return(x)
  }
  if(length(positions) >= n) {
    return(.ivx_empty_like(x))
  }

  keep <- setdiff(seq_len(n), as.integer(positions))
  x[as.integer(keep)]
}

# Runtime: O(n log n) from slice/remove operations.
.ivx_pop_positions <- function(x, positions, which = c("first", "all")) {
  which <- match.arg(which)

  if(length(positions) == 0L) {
    if(identical(which, "all")) {
      return(list(element = .ivx_empty_like(x), start = NULL, end = NULL, remaining = x))
    }
    return(list(element = NULL, start = NULL, end = NULL, remaining = x))
  }

  if(identical(which, "first")) {
    idx <- as.integer(positions[[1]])
    entry <- .ft_get_elem_at(x, idx)
    remaining <- .ivx_remove_positions(x, idx)
    return(list(element = entry$item, start = entry$start, end = entry$end, remaining = remaining))
  }

  matched <- .ivx_slice_positions(x, positions)
  remaining <- .ivx_remove_positions(x, positions)
  list(element = matched, start = NULL, end = NULL, remaining = remaining)
}

# Runtime: O(n log n) from reconstruction.
.ivx_apply_impl <- function(x, f, ...) {
  .ivx_assert_index(x)
  if(!is.function(f)) {
    stop("`FUN` must be a function.")
  }

  entries <- .ivx_entries(x)
  n <- length(entries)
  if(n == 0L) {
    return(x)
  }

  out_items <- vector("list", n)
  out_starts <- vector("list", n)
  out_ends <- vector("list", n)
  out_names <- if(is.null(names(entries))) rep("", n) else names(entries)
  endpoint_type <- .ivx_endpoint_type_state(x)

  for(i in seq_len(n)) {
    e <- entries[[i]]
    cur_name <- out_names[[i]]

    upd <- f(e$item, e$start, e$end, cur_name, ...)
    if(!is.list(upd)) {
      stop("`FUN` must return a list.")
    }

    if(length(upd) > 0L) {
      nm <- names(upd)
      if(is.null(nm) || any(is.na(nm)) || any(nm == "")) {
        stop("`FUN` must return a named list using only: item, start, end, name.")
      }
      if(anyDuplicated(nm) > 0L) {
        stop("`FUN` return list cannot contain duplicated field names.")
      }
      bad <- setdiff(nm, c("item", "start", "end", "name"))
      if(length(bad) > 0L) {
        stop("`FUN` returned unsupported field(s): ", paste(bad, collapse = ", "))
      }
    }

    item2 <- if("item" %in% names(upd)) upd[["item"]] else e$item
    start2 <- if("start" %in% names(upd)) upd[["start"]] else e$start
    end2 <- if("end" %in% names(upd)) upd[["end"]] else e$end

    norm <- .ivx_normalize_interval(start2, end2, endpoint_type = endpoint_type)
    endpoint_type <- norm$endpoint_type

    if("name" %in% names(upd)) {
      nm2 <- .ft_normalize_name(upd[["name"]])
      out_names[[i]] <- if(is.null(nm2)) "" else nm2
    }

    out_items[[i]] <- item2
    out_starts[[i]] <- norm$start
    out_ends[[i]] <- norm$end
  }

  if(any(out_names != "")) {
    names(out_items) <- out_names
  }

  as_interval_index(
    out_items,
    start = out_starts,
    end = out_ends,
    bounds = .ivx_bounds_state(x),
    monoids = .ivx_user_monoids(x)
  )
}

# Runtime: O(n log n) from sort + bulk build.
#' Build an Interval Index from elements and interval bounds
#'
#' @param x Elements to add.
#' @param start Scalar start endpoints (same length as `x`).
#' @param end Scalar end endpoints (same length as `x`).
#' @param bounds One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#' @return An `interval_index`.
#' @examples
#' ix <- as_interval_index(c("a", "b", "c"), start = c(1, 2, 2), end = c(3, 2, 4))
#' ix
#' as.list(find_point(ix, 2))
#' @export
as_interval_index <- function(x, start, end, bounds = "[)", monoids = NULL) {
  .ivx_build_from_items(as.list(x), start = start, end = end, bounds = bounds, monoids = monoids)
}

# Runtime: O(n log n) from sort + bulk build.
#' Construct an Interval Index
#'
#' @param ... Elements to add.
#' @param start Scalar start endpoints matching `...`.
#' @param end Scalar end endpoints matching `...`.
#' @param bounds One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#' @return An `interval_index`.
#' @examples
#' ix <- interval_index("a", "b", "c", start = c(1, 2, 2), end = c(3, 2, 4))
#' ix
#' @export
interval_index <- function(..., start = NULL, end = NULL, bounds = "[)", monoids = NULL) {
  as_interval_index(list(...), start = start, end = end, bounds = bounds, monoids = monoids)
}

# Runtime: O(log n) near split point depth.
#' Insert an element into an interval index
#'
#' @method insert interval_index
#' @param x An `interval_index`.
#' @param element Element to insert.
#' @param start Scalar start endpoint.
#' @param end Scalar end endpoint.
#' @param name Optional element name.
#' @param ... Unused.
#' @return Updated `interval_index`.
#' @export
insert.interval_index <- function(x, element, start, end, name = NULL, ...) {
  .ivx_assert_index(x)

  norm <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))
  entry <- .ivx_make_entry(element, norm$start, norm$end)

  if(!is.null(name)) {
    entry <- .ft_set_name(entry, name)
  }

  .ivx_insert_entry(x, entry, endpoint_type = norm$endpoint_type)
}

# Runtime: O(n log n) from reconstruction.
#' Apply a function over interval index entries
#'
#' @method fapply interval_index
#' @param X An `interval_index`.
#' @param FUN Function of `(item, start, end, name, ...)` returning a named
#'   list with fields from `item`, `start`, `end`, `name`.
#' @param ... Additional arguments passed to `FUN`.
#' @return A new `interval_index` with transformed entries.
#' @export
fapply.interval_index <- function(X, FUN, ...) {
  if(!is.function(FUN)) {
    stop("`FUN` must be a function.")
  }
  .ivx_apply_impl(X, FUN, ...)
}

# Runtime: O(n).
#' Coerce Interval Index to List
#'
#' @method as.list interval_index
#' @param x An `interval_index`.
#' @param ... Unused.
#' @return A plain list of payload elements in interval order.
#' @export
as.list.interval_index <- function(x, ...) {
  .ivx_assert_index(x)
  .ivx_extract_items(as.list.flexseq(x, ...))
}

# Runtime: O(1).
#' Interval Index Length
#'
#' @method length interval_index
#' @param x An `interval_index`.
#' @return Integer length.
#' @export
length.interval_index <- function(x) {
  as.integer(node_measure(x, ".size"))
}

# Runtime: O(n).
#' Get interval bounds in sequence order
#'
#' @param x An `interval_index`.
#' @return Data frame with list-columns `start` and `end`.
#' @export
interval_bounds <- function(x) {
  .ivx_assert_index(x)
  entries <- .ivx_entries(x)
  n <- length(entries)

  if(n == 0L) {
    return(data.frame(start = I(list()), end = I(list()), row.names = integer(0)))
  }

  starts <- unname(lapply(entries, function(e) e$start))
  ends <- unname(lapply(entries, function(e) e$end))
  data.frame(start = I(starts), end = I(ends), row.names = seq_len(n))
}

# Runtime: O(log n + c + k), where c = candidate count and k = matched count (worst-case O(n)).
#' Find intervals containing a point
#'
#' @param x An `interval_index`.
#' @param point Query point.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return An `interval_index` slice of matches.
#' @export
find_point <- function(x, point, bounds = NULL) {
  .ivx_assert_index(x)
  b <- .ivx_resolve_bounds(x, bounds)
  qp <- .ivx_normalize_endpoint(point, "point", endpoint_type = .ivx_endpoint_type_state(x))
  f <- .ivx_bounds_flags(b)

  # start > point can never contain point.
  entries <- .ivx_query_candidate_entries(
    x,
    upper = qp$value,
    upper_strict = FALSE
  )

  if(.ivx_is_fast_endpoint_type(qp$endpoint_type)) {
    include_start <- isTRUE(f$include_start)
    include_end <- isTRUE(f$include_end)
    return(.ivx_filter_slice(x, entries, function(e) {
      left_ok <- if(include_start) isTRUE(qp$value >= e$start) else isTRUE(qp$value > e$start)
      right_ok <- if(include_end) isTRUE(qp$value <= e$end) else isTRUE(qp$value < e$end)
      isTRUE(left_ok && right_ok)
    }))
  }

  .ivx_filter_slice(x, entries, function(e) {
    .ivx_contains_point(e$start, e$end, qp$value, b, qp$endpoint_type)
  })
}

# Runtime: O(log n + c + k), where c = candidate count and k = matched count (worst-case O(n)).
#' Find intervals overlapping a query interval
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return An `interval_index` slice of matches.
#' @export
find_overlaps <- function(x, start, end, bounds = NULL) {
  .ivx_assert_index(x)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))
  f <- .ivx_bounds_flags(b)
  touching_is_overlap <- isTRUE(f$include_start) && isTRUE(f$include_end)

  # For non-touching bounds, start == query_end cannot overlap.
  entries <- .ivx_query_candidate_entries(
    x,
    upper = q$end,
    upper_strict = !isTRUE(touching_is_overlap)
  )

  if(.ivx_is_fast_endpoint_type(q$endpoint_type)) {
    return(.ivx_filter_slice(x, entries, function(e) {
      a_before_b <- if(touching_is_overlap) isTRUE(e$end < q$start) else isTRUE(e$end <= q$start)
      b_before_a <- if(touching_is_overlap) isTRUE(q$end < e$start) else isTRUE(q$end <= e$start)
      !isTRUE(a_before_b || b_before_a)
    }))
  }

  .ivx_filter_slice(x, entries, function(e) {
    .ivx_overlaps_interval(e$start, e$end, q$start, q$end, b, q$endpoint_type)
  })
}

# Runtime: O(log n + c + k), where c = candidate count and k = matched count (worst-case O(n)).
#' Find intervals containing a query interval
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return An `interval_index` slice of matches.
#' @export
find_containing <- function(x, start, end, bounds = NULL) {
  .ivx_assert_index(x)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))
  f <- .ivx_bounds_flags(b)
  touching_is_overlap <- isTRUE(f$include_start) && isTRUE(f$include_end)

  # Containing intervals must start at or before query start.
  entries <- .ivx_query_candidate_entries(
    x,
    upper = q$start,
    upper_strict = FALSE
  )

  if(.ivx_is_fast_endpoint_type(q$endpoint_type)) {
    return(.ivx_filter_slice(x, entries, function(e) {
      contains <- isTRUE(e$start <= q$start) && isTRUE(e$end >= q$end)
      if(!contains) {
        return(FALSE)
      }
      a_before_b <- if(touching_is_overlap) isTRUE(e$end < q$start) else isTRUE(e$end <= q$start)
      b_before_a <- if(touching_is_overlap) isTRUE(q$end < e$start) else isTRUE(q$end <= e$start)
      !isTRUE(a_before_b || b_before_a)
    }))
  }

  .ivx_filter_slice(x, entries, function(e) {
    .ivx_overlaps_interval(e$start, e$end, q$start, q$end, b, q$endpoint_type) &&
      .ivx_contains_interval(e$start, e$end, q$start, q$end, q$endpoint_type)
  })
}

# Runtime: O(log n + c + k), where c = candidate count and k = matched count (worst-case O(n)).
#' Find intervals within a query interval
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return An `interval_index` slice of matches.
#' @export
find_within <- function(x, start, end, bounds = NULL) {
  .ivx_assert_index(x)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))
  f <- .ivx_bounds_flags(b)
  touching_is_overlap <- isTRUE(f$include_start) && isTRUE(f$include_end)

  # Within intervals must start inside query start-range.
  entries <- .ivx_query_candidate_entries(
    x,
    lower = q$start,
    lower_strict = FALSE,
    upper = q$end,
    upper_strict = !isTRUE(touching_is_overlap)
  )

  if(.ivx_is_fast_endpoint_type(q$endpoint_type)) {
    return(.ivx_filter_slice(x, entries, function(e) {
      within <- isTRUE(q$start <= e$start) && isTRUE(q$end >= e$end)
      if(!within) {
        return(FALSE)
      }
      a_before_b <- if(touching_is_overlap) isTRUE(e$end < q$start) else isTRUE(e$end <= q$start)
      b_before_a <- if(touching_is_overlap) isTRUE(q$end < e$start) else isTRUE(q$end <= e$start)
      !isTRUE(a_before_b || b_before_a)
    }))
  }

  .ivx_filter_slice(x, entries, function(e) {
    .ivx_overlaps_interval(e$start, e$end, q$start, q$end, b, q$endpoint_type) &&
      .ivx_contains_interval(q$start, q$end, e$start, e$end, q$endpoint_type)
  })
}

# Runtime: O(n log n) from matching + immutable rebuild.
#' Pop overlapping intervals
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param which One of `"first"` or `"all"`.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return For `which = "first"`, a list with `element`, `start`, `end`, `remaining`.
#'   For `which = "all"`, `element` is an `interval_index` slice and
#'   `start`/`end` are `NULL`.
#' @export
pop_overlaps <- function(x, start, end, which = c("first", "all"), bounds = NULL) {
  .ivx_assert_index(x)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))

  pos <- .ivx_match_positions(x, function(e) {
    .ivx_overlaps_interval(e$start, e$end, q$start, q$end, b, q$endpoint_type)
  })

  .ivx_pop_positions(x, pos, which = which)
}

# Runtime: O(n log n) from matching + immutable rebuild.
#' Pop intervals containing a query interval
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param which One of `"first"` or `"all"`.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return For `which = "first"`, a list with `element`, `start`, `end`, `remaining`.
#'   For `which = "all"`, `element` is an `interval_index` slice and
#'   `start`/`end` are `NULL`.
#' @export
pop_containing <- function(x, start, end, which = c("first", "all"), bounds = NULL) {
  .ivx_assert_index(x)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))

  pos <- .ivx_match_positions(x, function(e) {
    .ivx_overlaps_interval(e$start, e$end, q$start, q$end, b, q$endpoint_type) &&
      .ivx_contains_interval(e$start, e$end, q$start, q$end, q$endpoint_type)
  })

  .ivx_pop_positions(x, pos, which = which)
}

# Runtime: O(n log n) from matching + immutable rebuild.
#' Pop intervals within a query interval
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param which One of `"first"` or `"all"`.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return For `which = "first"`, a list with `element`, `start`, `end`, `remaining`.
#'   For `which = "all"`, `element` is an `interval_index` slice and
#'   `start`/`end` are `NULL`.
#' @export
pop_within <- function(x, start, end, which = c("first", "all"), bounds = NULL) {
  .ivx_assert_index(x)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))

  pos <- .ivx_match_positions(x, function(e) {
    .ivx_overlaps_interval(e$start, e$end, q$start, q$end, b, q$endpoint_type) &&
      .ivx_contains_interval(q$start, q$end, e$start, e$end, q$endpoint_type)
  })

  .ivx_pop_positions(x, pos, which = which)
}

#' Plot an Interval Index Tree
#'
#' @method plot interval_index
#' @param x An `interval_index`.
#' @param ... Passed to the internal tree plotting routine.
#' @export
# Runtime: O(n) to build plot graph data.
plot.interval_index <- function(x, ...) {
  plot.flexseq(x, ...)
}
