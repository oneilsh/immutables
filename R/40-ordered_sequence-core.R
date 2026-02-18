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
.as_ordered_multiset <- function(x, key_type) {
  if(!is_structural_node(x)) {
    stop("Expected a structural tree node.")
  }
  class(x) <- unique(c("ordered_multiset", "ordered_sequence", "flexseq", setdiff(class(x), "list")))
  attr(x, "oms_key_type") <- key_type
  x
}

# Runtime: O(1).
.ord_wrap_like <- function(template, tree, key_type = NULL) {
  resolved_key_type <- if(is.null(key_type)) .oms_key_type_state(template) else key_type
  if(inherits(template, "ordered_multiset")) {
    return(.as_ordered_multiset(tree, key_type = resolved_key_type))
  }
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
.oms_build_from_items <- function(items, keys = NULL, monoids = NULL, concrete_class = c("ordered_sequence", "ordered_multiset")) {
  concrete_class <- match.arg(concrete_class)
  n <- length(items)

  wrap <- if(concrete_class == "ordered_multiset") .as_ordered_multiset else .as_ordered_sequence

  if(n == 0L) {
    if(!is.null(keys) && length(as.list(keys)) > 0L) {
      stop("`keys` must be empty when no elements are supplied.")
    }
    base <- as_flexseq(list(), monoids = .oms_merge_monoids(monoids))
    return(wrap(base, key_type = NULL))
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
  wrap(base, key_type = key_type)
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
.oms_assert_compat <- function(x, y) {
  .oms_assert_set(x)
  .oms_assert_set(y)

  tx <- .oms_key_type_state(x)
  ty <- .oms_key_type_state(y)
  if(!identical(tx, ty) && !(is.null(tx) || is.null(ty))) {
    stop("Ordered key types are incompatible.")
  }

  invisible(TRUE)
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

# Runtime: O(n).
.oms_group_end <- function(entries, start_idx, key_type) {
  n <- length(entries)
  k <- entries[[start_idx]]$key
  j <- start_idx
  while(j <= n && .oms_compare_key(entries[[j]]$key, k, key_type) == 0L) {
    j <- j + 1L
  }
  as.integer(j - 1L)
}

# Runtime: O(1).
.oms_merge_engine <- function() {
  engine <- getOption("immutables.oms.merge_engine", "auto")
  if(is.null(engine)) {
    engine <- "auto"
  }
  if(!is.character(engine) || length(engine) != 1L || is.na(engine)) {
    stop("`options(immutables.oms.merge_engine=...)` must be one of: 'auto', 'cpp', 'legacy_r'.")
  }
  engine <- as.character(engine)
  if(!engine %in% c("auto", "cpp", "legacy_r")) {
    stop("`options(immutables.oms.merge_engine=...)` must be one of: 'auto', 'cpp', 'legacy_r'.")
  }
  engine
}

# Runtime: O(n + m) key merge scan + O(n + m) bulk build.
.oms_set_merge_legacy_r <- function(x, y, mode, result_key_type) {
  ex <- .oms_entries(x)
  ey <- .oms_entries(y)
  nx <- length(ex)
  ny <- length(ey)
  key_type <- .oms_key_type_state(x)
  ms <- attr(x, "monoids", exact = TRUE)

  out <- vector("list", nx + ny)
  out_len <- 0L
  i <- 1L
  j <- 1L

  append_entries <- function(src, start, end_incl) {
    if(start <= end_incl) {
      for(k in start:end_incl) {
        out_len <<- out_len + 1L
        out[[out_len]] <<- src[[k]]
      }
    }
  }

  while(i <= nx || j <= ny) {
    if(i > nx) {
      if(mode == "union") {
        append_entries(ey, j, ny)
      }
      break
    }
    if(j > ny) {
      if(mode %in% c("union", "difference")) {
        append_entries(ex, i, nx)
      }
      break
    }

    cmp <- .oms_compare_key(ex[[i]]$key, ey[[j]]$key, key_type)

    if(cmp < 0L) {
      if(mode %in% c("union", "difference")) {
        ie <- .oms_group_end(ex, i, key_type)
        append_entries(ex, i, ie)
        i <- ie + 1L
      } else {
        i <- .oms_group_end(ex, i, key_type) + 1L
      }
      next
    }

    if(cmp > 0L) {
      if(mode == "union") {
        je <- .oms_group_end(ey, j, key_type)
        append_entries(ey, j, je)
        j <- je + 1L
      } else {
        j <- .oms_group_end(ey, j, key_type) + 1L
      }
      next
    }

    ie <- .oms_group_end(ex, i, key_type)
    je <- .oms_group_end(ey, j, key_type)

    cx <- ie - i + 1L
    cy <- je - j + 1L

    if(mode == "intersection") {
      k <- min(cx, cy)
      if(k > 0L) {
        append_entries(ex, i, i + k - 1L)
      }
    } else if(mode == "difference") {
      k <- min(cx, cy)
      if(cx > k) {
        append_entries(ex, i + k, ie)
      }
    } else {
      append_entries(ex, i, ie)
      if(cy > cx) {
        append_entries(ey, j, j + (cy - cx) - 1L)
      }
    }

    i <- ie + 1L
    j <- je + 1L
  }

  if(out_len == 0L) {
    return(.ord_wrap_like(x, .oms_empty_tree_like(x), key_type = result_key_type))
  }
  out <- out[seq_len(out_len)]
  out_tree <- .oms_tree_from_ordered_entries(out, ms)
  .ord_wrap_like(x, out_tree, key_type = result_key_type)
}

# Runtime: O(n + m) key merge + O(n + m) bulk build.
.oms_set_merge_cpp <- function(x, y, mode, result_key_type) {
  ms <- attr(x, "monoids", exact = TRUE)
  key_type <- .oms_key_type_state(x)
  out_tree <- .as_flexseq(.ft_cpp_oms_set_merge(x, y, mode, ms, key_type))
  .ord_wrap_like(x, out_tree, key_type = result_key_type)
}

# Runtime: O(n + m) in the selected engine.
.oms_set_merge <- function(x, y, mode = c("union", "intersection", "difference")) {
  mode <- match.arg(mode)
  .oms_assert_compat(x, y)

  tx <- .oms_key_type_state(x)
  ty <- .oms_key_type_state(y)
  result_key_type <- if(!is.null(tx)) tx else ty

  nx <- length(x)
  ny <- length(y)
  if(nx == 0L) {
    if(mode == "union") {
      return(.ord_wrap_like(x, .as_flexseq(y), key_type = result_key_type))
    }
    return(.ord_wrap_like(x, .oms_empty_tree_like(x), key_type = result_key_type))
  }
  if(ny == 0L) {
    if(mode == "intersection") {
      return(.ord_wrap_like(x, .oms_empty_tree_like(x), key_type = result_key_type))
    }
    return(.ord_wrap_like(x, .as_flexseq(x), key_type = result_key_type))
  }

  engine <- .oms_merge_engine()
  ms <- attr(x, "monoids", exact = TRUE)
  can_cpp <- .ft_cpp_can_use(ms)

  if(identical(engine, "cpp")) {
    if(!can_cpp) {
      stop("Ordered multiset merge engine 'cpp' requested, but C++ backend is unavailable.")
    }
    return(.oms_set_merge_cpp(x, y, mode, result_key_type))
  }

  if(identical(engine, "auto") && can_cpp) {
    return(.oms_set_merge_cpp(x, y, mode, result_key_type))
  }

  .oms_set_merge_legacy_r(x, y, mode, result_key_type)
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
  .oms_build_from_items(as.list(x), keys = keys, monoids = monoids, concrete_class = "ordered_sequence")
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
    left_plus <- push_back(s$left, entry)
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

  entry <- .oms_entries(x)[[idx]]
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

  entry <- .oms_entries(x)[[idx]]
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
#' @param if_missing Value returned when no matching key is present.
#' @return For `which = "first"`, raw stored element or `if_missing` when not
#'   found. For `which = "all"`, an ordered sequence with matching elements
#'   (possibly empty).
#' @export
peek_key <- function(x, key, which = c("first", "all"), if_missing = NULL) {
  which <- match.arg(which)
  span <- .oms_key_span(x, key)

  if(!identical(which, "all")) {
    if(!isTRUE(span$found)) {
      return(if_missing)
    }
    s <- split_around_by_predicate(x, function(v) v >= span$start, ".size")
    return(s$elem$item)
  }

  if(!isTRUE(span$found)) {
    return(.ord_wrap_like(x, .oms_empty_tree_like(x)))
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
#' @return List with `element`, `key`, and updated `sequence`.
#'   For `which = "first"`:
#'   - On match: `element` is the first matching item; `key` is its key.
#'   - On miss: `element = NULL`, `key = NULL`, `sequence = x`.
#'
#'   For `which = "all"`:
#'   - `element` is an `ordered_sequence` containing all matching items (in
#'   stable order). This may have size 0 (miss), 1 (single match), or >1
#'   (multiple matches).
#'   - `key` is the normalized key on match, otherwise `NULL`.
#'   - `sequence` is the original sequence with that entire key-run removed
#'   (or unchanged on miss).
#' @export
pop_key <- function(x, key, which = c("first", "all")) {
  which <- match.arg(which)
  span <- .oms_key_span(x, key)

  if(!isTRUE(span$found)) {
    if(identical(which, "all")) {
      return(list(element = .ord_wrap_like(x, .oms_empty_tree_like(x)), key = NULL, sequence = x))
    }
    return(list(element = NULL, key = NULL, sequence = x))
  }

  if(!identical(which, "all")) {
    s <- split_around_by_predicate(x, function(v) v >= span$start, ".size")
    out <- concat_trees(s$left, s$right)
    seq_out <- .ord_wrap_like(x, out)
    return(list(element = s$elem$item, key = s$elem$key, sequence = seq_out))
  }
  parts <- .oms_slice_key_span(x, span$start, span$end_excl)
  list(
    element = .ord_wrap_like(x, parts$matched),
    key = span$key,
    sequence = .ord_wrap_like(x, parts$rest)
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

#' Apply over ordered sequence entries
#'
#' @rdname apply
#' @method apply ordered_sequence
#' @export
apply.ordered_sequence <- function(X, MARGIN = NULL, FUN = NULL, ...) {
  if(is.null(FUN)) {
    if(is.function(MARGIN)) {
      FUN <- MARGIN
      MARGIN <- NULL
    } else {
      stop("`FUN` must be a function.")
    }
  }
  if(!is.null(MARGIN)) {
    stop("`MARGIN` is not used for ordered_sequence; call `apply(x, FUN, ...)`.")
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

  entries <- .oms_entries(x)[seq.int(start, end_excl - 1L)]
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

# Runtime: O(1).
#' Ordered sequence set operations are not supported
#'
#' @rdname union
#' @method union ordered_sequence
#' @export
union.ordered_sequence <- function(x, y, ...) {
  stop("`union()` is only defined for ordered_multiset bag semantics. Convert with `as_ordered_multiset()`.")
}

# Runtime: O(1).
#' @rdname union
#' @method intersect ordered_sequence
#' @export
intersect.ordered_sequence <- function(x, y, ...) {
  stop("`intersect()` is only defined for ordered_multiset bag semantics. Convert with `as_ordered_multiset()`.")
}

# Runtime: O(1).
#' @rdname union
#' @method setdiff ordered_sequence
#' @export
setdiff.ordered_sequence <- function(x, y, ...) {
  stop("`setdiff()` is only defined for ordered_multiset bag semantics. Convert with `as_ordered_multiset()`.")
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
