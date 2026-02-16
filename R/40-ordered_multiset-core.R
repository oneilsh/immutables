# Runtime: O(1).
.oms_assert_key_fun <- function(key) {
  if(!is.function(key)) {
    stop("`key` must be a function.")
  }
  key
}

# Runtime: O(1).
.oms_key_id <- function(key) {
  txt <- paste(capture.output(dput(key)), collapse = "")
  paste0("oms_key:", txt)
}

# Runtime: O(1).
.oms_assert_set <- function(x) {
  if(!inherits(x, "ordered_multiset") || !is_structural_node(x)) {
    stop("`x` must be an ordered_multiset.")
  }
  invisible(TRUE)
}

# Runtime: O(1).
.oms_key_fun <- function(x) {
  key <- attr(x, "oms_key_fun", exact = TRUE)
  if(!is.function(key)) {
    stop("ordered_multiset is missing a valid `oms_key_fun` attribute.")
  }
  key
}

# Runtime: O(1).
.oms_key_type_state <- function(x) {
  attr(x, "oms_key_type", exact = TRUE)
}

# Runtime: O(1).
.oms_key_id_state <- function(x) {
  id <- attr(x, "oms_key_id", exact = TRUE)
  if(!is.character(id) || length(id) != 1L || is.na(id) || id == "") {
    stop("ordered_multiset is missing a valid `oms_key_id` attribute.")
  }
  id
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

# Runtime: O(1).
.as_ordered_multiset <- function(x, key, key_id, key_type, next_seq = NULL) {
  if(!is_structural_node(x)) {
    stop("Expected a structural tree node.")
  }
  class(x) <- unique(c("ordered_multiset", "flexseq", setdiff(class(x), "list")))
  attr(x, "oms_key_fun") <- key
  attr(x, "oms_key_id") <- key_id
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
.oms_apply_key <- function(key_fun, item, current_key_type = NULL) {
  key_raw <- key_fun(item)
  norm <- .oms_normalize_key(key_raw)
  key_type <- .oms_validate_key_type(current_key_type, norm$key_type)
  list(key = norm$key, key_type = key_type)
}

# Runtime: O(n log n) from tree construction after O(n log n) ordering.
.oms_build_from_items <- function(items, key, monoids = NULL) {
  key <- .oms_assert_key_fun(key)
  n <- length(items)

  if(n == 0L) {
    base <- as_flexseq(list(), monoids = .oms_merge_monoids(monoids))
    return(.as_ordered_multiset(
      base,
      key = key,
      key_id = .oms_key_id(key),
      key_type = NULL,
      next_seq = 1
    ))
  }

  entries <- vector("list", n)
  key_type <- NULL
  for(i in seq_len(n)) {
    k <- .oms_apply_key(key, items[[i]], current_key_type = key_type)
    key_type <- k$key_type
    entries[[i]] <- .oms_make_entry(items[[i]], k$key, i)
  }

  ord <- if(key_type == "numeric") {
    order(vapply(entries, function(e) e$key, numeric(1)), vapply(entries, function(e) e$seq_id, numeric(1)))
  } else if(key_type == "character") {
    order(vapply(entries, function(e) e$key, character(1)), vapply(entries, function(e) e$seq_id, numeric(1)))
  } else {
    order(vapply(entries, function(e) as.integer(isTRUE(e$key)), integer(1)), vapply(entries, function(e) e$seq_id, numeric(1)))
  }
  entries <- entries[ord]

  base <- as_flexseq(entries, monoids = .oms_merge_monoids(monoids))
  .as_ordered_multiset(
    base,
    key = key,
    key_id = .oms_key_id(key),
    key_type = key_type,
    next_seq = n + 1
  )
}

# Runtime: O(1).
.oms_wrap_tree <- function(template, tree, next_seq = NULL, key_type = NULL) {
  key <- .oms_key_fun(template)
  key_id <- .oms_key_id_state(template)
  resolved_key_type <- if(is.null(key_type)) .oms_key_type_state(template) else key_type
  .as_ordered_multiset(tree, key = key, key_id = key_id, key_type = resolved_key_type, next_seq = next_seq)
}

# Runtime: O(1).
.oms_empty_tree_like <- function(template) {
  ms <- attr(template, "monoids", exact = TRUE)
  .as_flexseq(measured_empty(ms))
}

# Runtime: O(log n) near split points.
.oms_slice_tree <- function(x, start, end_incl) {
  n <- length(x)
  start <- as.integer(start)
  end_incl <- as.integer(end_incl)
  if(start > end_incl || start > n) {
    return(.oms_empty_tree_like(x))
  }
  if(start < 1L) {
    start <- 1L
  }
  if(end_incl > n) {
    end_incl <- n
  }
  if(start == 1L && end_incl == n) {
    return(.as_flexseq(x))
  }

  s1 <- split_by_predicate(x, function(v) v >= start, ".size")
  span_len <- as.integer(end_incl - start + 1L)
  s2 <- split_by_predicate(s1$right, function(v) v >= (span_len + 1L), ".size")
  s2$left
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

  kx <- .oms_key_id_state(x)
  ky <- .oms_key_id_state(y)
  if(!identical(kx, ky)) {
    stop("Ordered multiset key extractors are incompatible. Rebuild with the same `key` function.")
  }

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

# Runtime: O(n + m) key merge scan + O(g log n) tree slicing/concats, where g is selected key-group count.
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

  ex <- .oms_entries(x)
  ey <- .oms_entries(y)
  key_type <- .oms_key_type_state(x)

  out_tree <- .oms_empty_tree_like(x)
  out_len <- 0L
  i <- 1L
  j <- 1L

  append_range <- function(src_tree, start, end_incl) {
    if(start <= end_incl) {
      piece <- .oms_slice_tree(src_tree, start, end_incl)
      if(out_len == 0L) {
        out_tree <<- piece
      } else {
        out_tree <<- concat_trees(out_tree, piece)
      }
      out_len <<- as.integer(out_len + (end_incl - start + 1L))
    }
  }

  while(i <= nx || j <= ny) {
    if(i > nx) {
      if(mode == "union") {
        append_range(y, j, ny)
      }
      break
    }
    if(j > ny) {
      if(mode %in% c("union", "difference")) {
        append_range(x, i, nx)
      }
      break
    }

    cmp <- .oms_compare_key(ex[[i]]$key, ey[[j]]$key, key_type)

    if(cmp < 0L) {
      if(mode %in% c("union", "difference")) {
        ie <- .oms_group_end(ex, i, key_type)
        append_range(x, i, ie)
        i <- ie + 1L
      } else {
        i <- .oms_group_end(ex, i, key_type) + 1L
      }
      next
    }

    if(cmp > 0L) {
      if(mode == "union") {
        je <- .oms_group_end(ey, j, key_type)
        append_range(y, j, je)
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
        append_range(x, i, i + k - 1L)
      }
    } else if(mode == "difference") {
      k <- min(cx, cy)
      if(cx > k) {
        append_range(x, i + k, ie)
      }
    } else {
      append_range(x, i, ie)
      if(cy > cx) {
        append_range(y, j, j + (cy - cx) - 1L)
      }
    }

    i <- ie + 1L
    j <- je + 1L
  }

  .oms_wrap_tree(x, out_tree, next_seq = out_len + 1L, key_type = result_key_type)
}

# Runtime: O(n log n) from build and ordering.
#' Build an Ordered Multiset from elements
#'
#' @param x Elements to add.
#' @param key Function extracting a scalar key from each element.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#' @return An `ordered_multiset`.
#' @examples
#' ms <- as_ordered_multiset(c(4, 1, 2, 1), key = identity)
#' ms
#' count_key(ms, 1)
#' @export
as_ordered_multiset <- function(x, key = identity, monoids = NULL) {
  .oms_build_from_items(as.list(x), key = key, monoids = monoids)
}

# Runtime: O(n log n) from build and ordering.
#' Construct an Ordered Multiset
#'
#' @param ... Elements to add.
#' @param key Function extracting a scalar key from each element.
#' @param monoids Optional additional named list of `measure_monoid` objects.
#' @return An `ordered_multiset`.
#' @examples
#' ms <- ordered_multiset("bb", "a", "ccc", key = nchar)
#' ms
#' lower_bound(ms, 2)
#' @export
ordered_multiset <- function(..., key = identity, monoids = NULL) {
  as_ordered_multiset(list(...), key = key, monoids = monoids)
}

# Runtime: O(log n) near insertion/split point depth.
#' Insert an element into an ordered multiset
#'
#' @param x An `ordered_multiset`.
#' @param element Element to insert.
#' @return Updated `ordered_multiset`.
#' @export
insert_ms <- function(x, element) {
  .oms_assert_set(x)
  key_fun <- .oms_key_fun(x)
  key_type <- .oms_key_type_state(x)
  k <- .oms_apply_key(key_fun, element, current_key_type = key_type)

  seq_id <- .oms_next_seq(x)
  entry <- .oms_make_entry(element, k$key, seq_id)

  idx <- .oms_bound_index(x, k$key, strict = TRUE)
  s <- split_by_predicate(x, function(v) v >= idx, ".size")
  left_plus <- append(s$left, entry)
  out <- concat_trees(left_plus, s$right)

  .oms_wrap_tree(x, out, next_seq = seq_id + 1L, key_type = k$key_type)
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
#' @return `list(found, index, element, key, seq_id)`.
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
#' @return `list(found, index, element, key, seq_id)`.
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
#' Count multiplicity of one key
#'
#' @param x An `ordered_multiset`.
#' @param key_value Query key.
#' @return Integer count.
#' @export
count_key <- function(x, key_value) {
  .oms_assert_set(x)
  l <- .oms_bound_index(x, key_value, strict = FALSE)
  u <- .oms_bound_index(x, key_value, strict = TRUE)
  as.integer(max(0L, u - l))
}

# Runtime: O(log n).
#' Count elements in a key range
#'
#' @param x An `ordered_multiset`.
#' @param lo Lower bound key.
#' @param hi Upper bound key.
#' @param include_lo Include lower bound when `TRUE`.
#' @param include_hi Include upper bound when `TRUE`.
#' @return Integer count.
#' @export
count_between <- function(x, lo, hi, include_lo = TRUE, include_hi = TRUE) {
  .oms_assert_set(x)
  include_lo <- .oms_coerce_lgl_scalar(include_lo, "include_lo")
  include_hi <- .oms_coerce_lgl_scalar(include_hi, "include_hi")

  start <- .oms_range_start_index(x, lo, include_lo)
  end_excl <- .oms_range_end_exclusive_index(x, hi, include_hi)
  as.integer(max(0L, end_excl - start))
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

# Runtime: O(n + m) key merge scan + O(g log n) tree slicing/concats, where g is selected key-group count.
#' Multiset union (bag semantics)
#'
#' @param x Left `ordered_multiset`.
#' @param y Right `ordered_multiset`.
#' @return `ordered_multiset` where multiplicity is `max(count_x, count_y)`.
#' @export
union_ms <- function(x, y) {
  .oms_set_merge(x, y, mode = "union")
}

# Runtime: O(n + m) key merge scan + O(g log n) tree slicing/concats, where g is selected key-group count.
#' Multiset intersection (bag semantics)
#'
#' @param x Left `ordered_multiset`.
#' @param y Right `ordered_multiset`.
#' @return `ordered_multiset` where multiplicity is `min(count_x, count_y)`.
#' @export
intersection_ms <- function(x, y) {
  .oms_set_merge(x, y, mode = "intersection")
}

# Runtime: O(n + m) key merge scan + O(g log n) tree slicing/concats, where g is selected key-group count.
#' Multiset difference (bag semantics)
#'
#' @param x Left `ordered_multiset`.
#' @param y Right `ordered_multiset`.
#' @return `ordered_multiset` where multiplicity is `pmax(count_x - count_y, 0)`.
#' @export
difference_ms <- function(x, y) {
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
