# Runtime: O(1).
.oms_assert_set <- function(x) {
  if(!inherits(x, "ordered_multiset") || !is_structural_node(x)) {
    stop("`x` must be an ordered_multiset.")
  }
  invisible(TRUE)
}

# Runtime: O(1).
.oms_key_type_state <- function(x) {
  attr(x, "oms_key_type", exact = TRUE)
}

# Runtime: O(1).
.oms_next_seq <- function(x) {
  n <- attr(x, "oms_next_seq", exact = TRUE)
  if(!is.numeric(n) || length(n) != 1L || is.na(n)) {
    stop("ordered_multiset is missing valid `oms_next_seq` attribute.")
  }
  as.numeric(n)
}

# Runtime: O(1).
.oms_make_entry <- function(item, key_value, seq_id) {
  list(item = item, key = key_value, seq_id = as.numeric(seq_id))
}

# Runtime: O(n log n) from ordering by key then seq_id.
.oms_order_entries <- function(entries, key_type) {
  if(length(entries) == 0L) {
    return(entries)
  }
  ord <- if(key_type == "numeric") {
    order(vapply(entries, function(e) e$key, numeric(1)), vapply(entries, function(e) e$seq_id, numeric(1)))
  } else if(key_type == "character") {
    order(vapply(entries, function(e) e$key, character(1)), vapply(entries, function(e) e$seq_id, numeric(1)))
  } else {
    order(vapply(entries, function(e) as.integer(isTRUE(e$key)), integer(1)), vapply(entries, function(e) e$seq_id, numeric(1)))
  }
  entries[ord]
}

# Runtime: O(1).
.as_ordered_multiset <- function(x, key_type, next_seq = NULL) {
  if(!is_structural_node(x)) {
    stop("Expected a structural tree node.")
  }
  class(x) <- unique(c("ordered_multiset", "flexseq", setdiff(class(x), "list")))
  attr(x, "oms_key_type") <- key_type
  if(is.null(next_seq)) {
    next_seq <- as.numeric(node_measure(x, ".size")) + 1
  }
  attr(x, "oms_next_seq") <- as.numeric(next_seq)
  x
}

# Runtime: O(1).
.oms_validate_key_type <- function(oms_key_type, new_key_type) {
  if(is.null(oms_key_type)) {
    return(new_key_type)
  }
  if(!identical(oms_key_type, new_key_type)) {
    stop("Incompatible key type for this ordered_multiset.")
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
    return(.as_ordered_multiset(
      base,
      key_type = NULL,
      next_seq = 1
    ))
  }

  if(is.null(keys)) {
    stop("`keys` is required when elements are supplied.")
  }
  keys <- .oms_as_key_list(keys, n)

  entries <- vector("list", n)
  key_type <- NULL
  for(i in seq_len(n)) {
    norm <- .oms_normalize_key(keys[[i]])
    key_type <- .oms_validate_key_type(key_type, norm$key_type)
    entries[[i]] <- .oms_make_entry(items[[i]], norm$key, i)
  }

  entries <- .oms_order_entries(entries, key_type)

  merged_monoids <- .oms_merge_monoids(monoids)
  base <- .oms_tree_from_ordered_entries(entries, merged_monoids)
  .as_ordered_multiset(
    base,
    key_type = key_type,
    next_seq = n + 1
  )
}

# Runtime: O(1).
.oms_wrap_tree <- function(template, tree, next_seq = NULL, key_type = NULL) {
  resolved_key_type <- if(is.null(key_type)) .oms_key_type_state(template) else key_type
  .as_ordered_multiset(tree, key_type = resolved_key_type, next_seq = next_seq)
}

# Runtime: O(1).
.oms_empty_tree_like <- function(template) {
  ms <- attr(template, "monoids", exact = TRUE)
  .as_flexseq(measured_empty(ms))
}

# Runtime: O(n) for ordered entries.
.oms_tree_from_ordered_entries <- function(entries, monoids) {
  if(.ft_cpp_can_use(monoids)) {
    return(.as_flexseq(.ft_cpp_tree_from_sorted(entries, monoids)))
  }
  .ft_tree_from_list_linear(entries, monoids)
}

# Runtime: O(1).
.oms_entry_key_cmp <- function(entry_key, key_value, key_type) {
  .oms_compare_key(entry_key, key_value, key_type)
}

# Runtime: O(log n) near locate point depth.
.oms_bound_index <- function(x, key_value, strict = FALSE) {
  .oms_assert_set(x)
  n <- length(x)
  if(n == 0L) {
    return(1L)
  }

  key_type <- .oms_key_type_state(x)
  norm <- .oms_normalize_key(key_value)
  .oms_validate_key_type(key_type, norm$key_type)

  pred <- if(!isTRUE(strict)) {
    function(v) {
      isTRUE(v$has) && .oms_compare_key(v$key, norm$key, v$key_type) >= 0L
    }
  } else {
    function(v) {
      isTRUE(v$has) && .oms_compare_key(v$key, norm$key, v$key_type) > 0L
    }
  }

  loc <- locate_by_predicate(x, pred, ".oms_max_key", include_metadata = TRUE)
  if(!isTRUE(loc$found)) {
    return(as.integer(n + 1L))
  }
  as.integer(loc$metadata$index)
}

# Runtime: O(1).
.oms_assert_compat <- function(x, y) {
  .oms_assert_set(x)
  .oms_assert_set(y)

  tx <- .oms_key_type_state(x)
  ty <- .oms_key_type_state(y)
  if(!identical(tx, ty) && !(is.null(tx) || is.null(ty))) {
    stop("Ordered multiset key types are incompatible.")
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

    gx <- ex[i:ie]
    gy <- ey[j:je]
    cx <- length(gx)
    cy <- length(gy)

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
    return(.oms_wrap_tree(x, .oms_empty_tree_like(x), next_seq = 1L, key_type = result_key_type))
  }
  out <- out[seq_len(out_len)]
  out_tree <- .oms_tree_from_ordered_entries(out, ms)
  .oms_wrap_tree(x, out_tree, next_seq = out_len + 1L, key_type = result_key_type)
}

# Runtime: O(n + m) key merge + O(n + m) bulk build.
.oms_set_merge_cpp <- function(x, y, mode, result_key_type) {
  ms <- attr(x, "monoids", exact = TRUE)
  key_type <- .oms_key_type_state(x)
  out_tree <- .as_flexseq(.ft_cpp_oms_set_merge(x, y, mode, ms, key_type))
  out_n <- as.integer(node_measure(out_tree, ".size"))
  .oms_wrap_tree(x, out_tree, next_seq = out_n + 1L, key_type = result_key_type)
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
      return(.oms_wrap_tree(x, .as_flexseq(y), next_seq = ny + 1L, key_type = result_key_type))
    }
    return(.oms_wrap_tree(x, .oms_empty_tree_like(x), next_seq = 1L, key_type = result_key_type))
  }
  if(ny == 0L) {
    if(mode == "intersection") {
      return(.oms_wrap_tree(x, .oms_empty_tree_like(x), next_seq = 1L, key_type = result_key_type))
    }
    return(.oms_wrap_tree(x, .as_flexseq(x), next_seq = nx + 1L, key_type = result_key_type))
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
#' Build an Ordered Multiset from elements
#'
#' @param x Elements to add.
#' @param keys Scalar key values matching `x` length.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#' @return An `ordered_multiset`.
#' @examples
#' ms <- as_ordered_multiset(c(4, 1, 2, 1), keys = c(4, 1, 2, 1))
#' ms
#' length(elements_between(ms, 1, 1))
#' @export
as_ordered_multiset <- function(x, keys = NULL, monoids = NULL) {
  .oms_build_from_items(as.list(x), keys = keys, monoids = monoids)
}

# Runtime: O(n log n) from build and ordering.
#' Construct an Ordered Multiset
#'
#' @param ... Elements to add.
#' @param keys Scalar key values matching `...` length.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#' @return An `ordered_multiset`.
#' @examples
#' ms <- ordered_multiset("bb", "a", "ccc", keys = c(2, 1, 3))
#' ms
#' lower_bound(ms, 2)
#' @export
ordered_multiset <- function(..., keys = NULL, monoids = NULL) {
  as_ordered_multiset(list(...), keys = keys, monoids = monoids)
}

# Runtime: O(log n) near insertion/split point depth.
#' @noRd
.oms_insert_impl <- function(x, element, key) {
  .oms_assert_set(x)
  norm <- .oms_normalize_key(key)
  key_type <- .oms_validate_key_type(.oms_key_type_state(x), norm$key_type)

  seq_id <- .oms_next_seq(x)
  entry <- .oms_make_entry(element, norm$key, seq_id)
  ms <- attr(x, "monoids", exact = TRUE)

  out <- if(.ft_cpp_can_use(ms)) {
    .as_flexseq(.ft_cpp_oms_insert(x, entry, ms, key_type))
  } else {
    # One-pass split on max-key measure: left keeps <= key, right keeps > key.
    s <- split_by_predicate(
      x,
      function(v) isTRUE(v$has) && .oms_compare_key(v$key, norm$key, v$key_type) > 0L,
      ".oms_max_key"
    )
    left_plus <- append(s$left, entry)
    concat_trees(left_plus, s$right)
  }

  .oms_wrap_tree(x, out, next_seq = seq_id + 1L, key_type = key_type)
}

# Runtime: O(log n) near insertion/split point depth.
#' @rdname insert
#' @param key Scalar key for `element`.
#' @return Updated `ordered_multiset`.
#' @export
insert.ordered_multiset <- function(x, element, key, ...) {
  .oms_insert_impl(x, element, key)
}

# Runtime: O(log n) near split points.
#' Delete one element by key
#'
#' @param x An `ordered_multiset`.
#' @param key_value Key value to delete.
#' @return Updated `ordered_multiset`.
#' @export
delete_one <- function(x, key_value) {
  .oms_assert_set(x)
  l <- .oms_bound_index(x, key_value, strict = FALSE)
  u <- .oms_bound_index(x, key_value, strict = TRUE)

  if(l >= u) {
    return(x)
  }

  s1 <- split_by_predicate(x, function(v) v >= l, ".size")
  s2 <- split_by_predicate(s1$right, function(v) v >= 2L, ".size")
  out <- concat_trees(s1$left, s2$right)
  .oms_wrap_tree(x, out, next_seq = .oms_next_seq(x))
}

# Runtime: O(log n) near split points.
#' Delete all elements by key
#'
#' @param x An `ordered_multiset`.
#' @param key_value Key value to delete.
#' @return Updated `ordered_multiset`.
#' @export
delete_all <- function(x, key_value) {
  .oms_assert_set(x)
  l <- .oms_bound_index(x, key_value, strict = FALSE)
  u <- .oms_bound_index(x, key_value, strict = TRUE)

  if(l >= u) {
    return(x)
  }

  s1 <- split_by_predicate(x, function(v) v >= l, ".size")
  span_len <- as.integer(u - l)
  s2 <- split_by_predicate(s1$right, function(v) v >= (span_len + 1L), ".size")
  out <- concat_trees(s1$left, s2$right)
  .oms_wrap_tree(x, out, next_seq = .oms_next_seq(x))
}

# Runtime: O(log n).
#' Find first element with key >= value
#'
#' @param x An `ordered_multiset`.
#' @param key_value Query key.
#' @return Named list with fields `found`, `index`, `element`, `key`, and
#'   `seq_id`. When no match exists, `found` is `FALSE` and the remaining fields
#'   are `NULL`.
#' @export
lower_bound <- function(x, key_value) {
  .oms_assert_set(x)
  idx <- .oms_bound_index(x, key_value, strict = FALSE)
  n <- length(x)

  if(idx > n) {
    return(list(found = FALSE, index = NULL, element = NULL, key = NULL, seq_id = NULL))
  }

  entry <- .oms_entries(x)[[idx]]
  list(found = TRUE, index = idx, element = entry$item, key = entry$key, seq_id = entry$seq_id)
}

# Runtime: O(log n).
#' Find first element with key > value
#'
#' @param x An `ordered_multiset`.
#' @param key_value Query key.
#' @return Named list with fields `found`, `index`, `element`, `key`, and
#'   `seq_id`. When no match exists, `found` is `FALSE` and the remaining fields
#'   are `NULL`.
#' @export
upper_bound <- function(x, key_value) {
  .oms_assert_set(x)
  idx <- .oms_bound_index(x, key_value, strict = TRUE)
  n <- length(x)

  if(idx > n) {
    return(list(found = FALSE, index = NULL, element = NULL, key = NULL, seq_id = NULL))
  }

  entry <- .oms_entries(x)[[idx]]
  list(found = TRUE, index = idx, element = entry$item, key = entry$key, seq_id = entry$seq_id)
}

# Runtime: O(log n).
#' Peek first element for one key
#'
#' Returns the earliest inserted element among entries whose key equals
#' `key_value`.
#'
#' @param x An `ordered_multiset`.
#' @param key_value Query key.
#' @return Raw stored element.
#' @export
peek_key <- function(x, key_value) {
  .oms_assert_set(x)
  norm <- .oms_normalize_key(key_value)
  key_type <- .oms_validate_key_type(.oms_key_type_state(x), norm$key_type)

  idx <- .oms_bound_index(x, norm$key, strict = FALSE)
  n <- length(x)
  if(idx > n) {
    stop("Key not found in ordered_multiset.")
  }

  s <- split_around_by_predicate(x, function(v) v >= idx, ".size")
  if(.oms_compare_key(s$elem$key, norm$key, key_type) != 0L) {
    stop("Key not found in ordered_multiset.")
  }
  s$elem$item
}

# Runtime: O(log n) near split point depth.
#' Extract first element for one key
#'
#' Removes and returns the earliest inserted element among entries whose key
#' equals `key_value`.
#'
#' @param x An `ordered_multiset`.
#' @param key_value Query key.
#' @return List with `element`, `key`, and updated `multiset`.
#' @export
extract_key <- function(x, key_value) {
  .oms_assert_set(x)
  norm <- .oms_normalize_key(key_value)
  key_type <- .oms_validate_key_type(.oms_key_type_state(x), norm$key_type)

  idx <- .oms_bound_index(x, norm$key, strict = FALSE)
  n <- length(x)
  if(idx > n) {
    stop("Key not found in ordered_multiset.")
  }

  s <- split_around_by_predicate(x, function(v) v >= idx, ".size")
  if(.oms_compare_key(s$elem$key, norm$key, key_type) != 0L) {
    stop("Key not found in ordered_multiset.")
  }

  out <- concat_trees(s$left, s$right)
  ms <- .oms_wrap_tree(x, out, next_seq = .oms_next_seq(x))
  list(element = s$elem$item, key = s$elem$key, multiset = ms)
}

# Runtime: O(n log n) total from traversal + reordering + rebuild.
.oms_apply_impl <- function(x, f, reset_ties = FALSE, ...) {
  .oms_assert_set(x)
  if(!is.function(f)) {
    stop("`FUN` must be a function.")
  }
  if(!is.logical(reset_ties) || length(reset_ties) != 1L || is.na(reset_ties)) {
    stop("`reset_ties` must be TRUE or FALSE.")
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

    upd <- f(e$item, e$key, e$seq_id, cur_name, ...)
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

    out[[i]] <- .oms_make_entry(
      item = item2,
      key_value = key2,
      seq_id = if(isTRUE(reset_ties)) i else e$seq_id
    )
  }

  if(any(out_names != "")) {
    names(out) <- out_names
  }

  out <- .oms_order_entries(out, key_type)
  ms <- attr(x, "monoids", exact = TRUE)
  out_tree <- .oms_tree_from_ordered_entries(out, ms)
  next_seq <- if(isTRUE(reset_ties)) as.numeric(n + 1L) else .oms_next_seq(x)
  .oms_wrap_tree(x, out_tree, next_seq = next_seq, key_type = key_type)
}

#' Apply over ordered multiset entries
#'
#' @rdname apply
#' @method apply ordered_multiset
#' @param reset_ties Logical; if `TRUE`, refreshes tie-break `seq_id` by
#'   current object order. If `FALSE` (default), preserves existing `seq_id`
#'   values.
#' @export
apply.ordered_multiset <- function(X, MARGIN = NULL, FUN = NULL, ..., reset_ties = FALSE) {
  if(is.null(FUN)) {
    if(is.function(MARGIN)) {
      FUN <- MARGIN
      MARGIN <- NULL
    } else {
      stop("`FUN` must be a function.")
    }
  }
  if(!is.null(MARGIN)) {
    stop("`MARGIN` is not used for ordered_multiset; call `apply(x, FUN, ...)`.")
  }
  .oms_apply_impl(X, FUN, reset_ties = reset_ties, ...)
}

# Runtime: O(log n + k), where k is output size.
#' Return elements in a key range
#'
#' @param x An `ordered_multiset`.
#' @param lo Lower bound key.
#' @param hi Upper bound key.
#' @param include_lo Include lower bound when `TRUE`.
#' @param include_hi Include upper bound when `TRUE`.
#' @return List of raw elements.
#' @export
elements_between <- function(x, lo, hi, include_lo = TRUE, include_hi = TRUE) {
  .oms_assert_set(x)
  include_lo <- .oms_coerce_lgl_scalar(include_lo, "include_lo")
  include_hi <- .oms_coerce_lgl_scalar(include_hi, "include_hi")

  start <- .oms_range_start_index(x, lo, include_lo)
  end_excl <- .oms_range_end_exclusive_index(x, hi, include_hi)

  if(end_excl <= start || start > length(x)) {
    return(list())
  }

  entries <- .oms_entries(x)[seq.int(start, end_excl - 1L)]
  .oms_extract_items(entries)
}

# Runtime: O(n + m) key merge scan + O(n + m) bulk build.
#' Multiset union (bag semantics)
#'
#' @rdname union
#' @method union ordered_multiset
#' @return `ordered_multiset` where multiplicity is `max(count_x, count_y)`.
#' @export
union.ordered_multiset <- function(x, y, ...) {
  .oms_set_merge(x, y, mode = "union")
}

# Runtime: O(n + m) key merge scan + O(n + m) bulk build.
#' Multiset intersection (bag semantics)
#'
#' @rdname union
#' @method intersect ordered_multiset
#' @return `ordered_multiset` where multiplicity is `min(count_x, count_y)`.
#' @export
intersect.ordered_multiset <- function(x, y, ...) {
  .oms_set_merge(x, y, mode = "intersection")
}

# Runtime: O(n + m) key merge scan + O(n + m) bulk build.
#' Multiset difference (bag semantics)
#'
#' @rdname union
#' @method setdiff ordered_multiset
#' @return `ordered_multiset` where multiplicity is `pmax(count_x - count_y, 0)`.
#' @export
setdiff.ordered_multiset <- function(x, y, ...) {
  .oms_set_merge(x, y, mode = "difference")
}

#' @method as.list ordered_multiset
#' @export
# Runtime: O(n).
as.list.ordered_multiset <- function(x, ...) {
  .oms_assert_set(x)
  .oms_extract_items(as.list.flexseq(x, ...))
}

#' @method length ordered_multiset
#' @export
# Runtime: O(1).
length.ordered_multiset <- function(x) {
  .oms_assert_set(x)
  as.integer(node_measure(x, ".size"))
}
