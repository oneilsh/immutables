# recursive fill helper for `.ft_to_list` to avoid repeated list concatenation.
# Runtime: O(n) in number of elements visited.
.ft_to_list_fill(x, st) %::% . : environment : .
.ft_to_list_fill(x, st) %as% {
  if(!is_structural_node(x)) {
    st$out[[st$pos]] <- x
    st$pos <- st$pos + 1L
    return(invisible(NULL))
  }
  if(x %isa% Empty) {
    return(invisible(NULL))
  }
  if(x %isa% Single) {
    .ft_to_list_fill(.subset2(x, 1), st)
    return(invisible(NULL))
  }
  if(x %isa% Deep) {
    .ft_to_list_fill(.subset2(x, "prefix"), st)
    .ft_to_list_fill(.subset2(x, "middle"), st)
    .ft_to_list_fill(.subset2(x, "suffix"), st)
    return(invisible(NULL))
  }
  for(el in x) {
    .ft_to_list_fill(el, st)
  }
  invisible(NULL)
}

# flatten a fingertree into its element sequence (left-to-right)
# Runtime: O(n) in number of elements.
.ft_to_list(x) %::% . : list
.ft_to_list(x) %as% {
  if(!is_structural_node(x)) {
    return(list(x))
  }
  n <- as.integer(node_measure(x, ".size"))
  if(n == 0L) {
    return(list())
  }
  st <- new.env(parent = emptyenv())
  st$out <- vector("list", n)
  st$pos <- 1L
  .ft_to_list_fill(x, st)
  st$out
}

# Runtime: O(m), where m = length(idx).
.ft_assert_int_indices(idx, n) %::% . : numeric : integer
.ft_assert_int_indices(idx, n) %as% {
  if(is.null(idx)) {
    stop("Index is required.")
  }
  if(!is.numeric(idx) || any(is.na(idx)) || any(idx != as.integer(idx))) {
    stop("Only non-missing integer indices are supported.")
  }
  idx <- as.integer(idx)
  if(length(idx) == 0L) {
    return(idx)
  }
  if(any(idx <= 0L)) {
    stop("Only positive integer indices are supported.")
  }
  if(any(idx > n)) {
    stop("Index out of bounds.")
  }
  idx
}

# Runtime: O(m), where m = length(idx).
.ft_assert_chr_indices(idx) %::% . : character
.ft_assert_chr_indices(idx) %as% {
  if(is.null(idx)) {
    stop("Index is required.")
  }
  if(!is.character(idx) || any(is.na(idx))) {
    stop("Only non-missing character indices are supported.")
  }
  idx
}

# Runtime: O(n) due `rep_len(..., n)`.
.ft_assert_lgl_indices(idx, n) %::% . : integer : logical
.ft_assert_lgl_indices(idx, n) %as% {
  if(is.null(idx)) {
    stop("Index is required.")
  }
  if(!is.logical(idx)) {
    stop("Only logical indices are supported.")
  }
  if(any(is.na(idx))) {
    stop("Logical indices cannot contain NA.")
  }
  if(length(idx) == 0L) {
    return(logical(0))
  }
  rep_len(idx, n)
}

# Runtime: O(n) worst-case in relevant input/subtree size.
.ft_true_positions(mask) %::% logical : integer
.ft_true_positions(mask) %as% {
  as.integer(which(mask))
}

# minimum character-query width at which full name->position map lookup is used.
# Runtime: O(1).
.ft_name_map_threshold() %::% integer
.ft_name_map_threshold() %as% {
  # Tuned internal constant: switch to one-pass name->position map lookup for
  # medium/large character queries; keep scalar find loop for short vectors.
  8L
}

# Runtime: O(target_len) in recycled output size.
.ft_recycle_values(values, target_len) %::% list : integer : list
.ft_recycle_values(values, target_len) %as% {
  if(target_len == 0L) {
    return(list())
  }
  if(length(values) == 0L) {
    stop("Replacement has length 0 but selected positions are non-empty.")
  }
  if(length(values) == target_len) {
    return(values)
  }
  rep(values, length.out = target_len)
}

# check whether replacement payloads provide any explicit name hints
# (outer list names, ft_name attrs, or inline scalar names()).
# Runtime: O(k), where k = number of replacement values.
.ft_values_have_name_hints(values) %::% list : logical
.ft_values_have_name_hints(values) %as% {
  vn <- names(values)
  if(!is.null(vn) && any(!is.na(vn) & vn != "")) {
    return(TRUE)
  }
  if(length(values) == 0L) {
    return(FALSE)
  }
  for(v in values) {
    if(!is.null(.ft_get_name(v)) || !is.null(.ft_name_from_value(v))) {
      return(TRUE)
    }
  }
  FALSE
}

# remove internal naming metadata from user-visible element returns.
# Runtime: O(1).
.ft_strip_name(el) %::% . : .
.ft_strip_name(el) %as% {
  attr(el, "ft_name") <- NULL
  el
}

# collect internal element names in left-to-right order.
# Runtime: O(n).
.ft_collect_names(t) %::% . : character
.ft_collect_names(t) %as% {
  els <- .ft_to_list(t)
  if(length(els) == 0L) {
    return(character(0))
  }
  vapply(els, function(el) {
    nm <- .ft_get_name(el)
    if(is.null(nm)) NA_character_ else nm
  }, character(1))
}

# validate global naming invariant:
# - unnamed tree: .named_count == 0
# - fully named tree: .named_count == .size and names are unique/non-empty
# Runtime: O(n) in the fully-named case (collect/scan all names).
# Intended use: correctness auditing in tests/debugging and explicit validators.
.ft_assert_name_state(t) %::% . : .
.ft_assert_name_state(t) %as% {
  if(!is_structural_node(t)) {
    return(invisible(TRUE))
  }
  n <- as.integer(node_measure(t, ".size"))
  nn <- as.integer(node_measure(t, ".named_count"))
  if(n == 0L) {
    if(nn != 0L) {
      stop("Invalid name state: empty tree has non-zero named element count.")
    }
    return(invisible(TRUE))
  }
  if(nn == 0L) {
    return(invisible(TRUE))
  }
  if(nn != n) {
    stop("Invalid name state: mixed named and unnamed elements are not allowed.")
  }
  nms <- .ft_collect_names(t)
  if(any(is.na(nms) | nms == "")) {
    stop("Invalid name state: all element names must be non-empty.")
  }
  if(anyDuplicated(nms) > 0L) {
    stop("Invalid name state: element names must be unique.")
  }
  invisible(TRUE)
}

# build a deterministic name -> position map for fully named trees.
# Runtime: O(n).
.ft_name_positions(t) %::% . : integer
.ft_name_positions(t) %as% {
  n <- as.integer(node_measure(t, ".size"))
  nn <- as.integer(node_measure(t, ".named_count"))
  if(n == 0L || nn == 0L) {
    stop("Tree has no element names.")
  }
  if(nn != n) {
    stop("Invalid name state: mixed named and unnamed elements are not allowed.")
  }
  nms <- .ft_collect_names(t)
  pos <- seq_len(length(nms))
  names(pos) <- nms
  pos
}

# build name->position map, using C++ when available.
# Runtime: O(n).
.ft_name_positions_fast(t) %::% . : integer
.ft_name_positions_fast(t) %as% {
  if(.ft_cpp_enabled()) {
    return(.ft_cpp_name_positions(t))
  }
  .ft_name_positions(t)
}

# measure contribution of one child in terms of leaf-element count.
# Runtime: O(1) using cached `.size` for structural children.
.ft_child_size(el) %::% . : integer
.ft_child_size(el) %as% {
  if(is_structural_node(el)) {
    return(as.integer(node_measure(el, ".size")))
  }
  1L
}

# recursive linear search for one name with early exit.
# Runtime: O(n) worst-case, O(k) until first match.
.ft_find_name_position_impl(x, target, offset) %::% . : character : integer : integer
.ft_find_name_position_impl(x, target, offset = 0L) %as% {
  if(!is_structural_node(x)) {
    nm <- .ft_get_name(x)
    if(!is.null(nm) && identical(nm, target)) {
      return(offset + 1L)
    }
    return(NA_integer_)
  }
  if(x %isa% Empty) {
    return(NA_integer_)
  }
  if(x %isa% Single) {
    return(.ft_find_name_position_impl(.subset2(x, 1), target, offset))
  }
  if(x %isa% Deep) {
    p <- .ft_find_name_position_impl(.subset2(x, "prefix"), target, offset)
    if(!is.na(p)) {
      return(p)
    }
    offset <- offset + as.integer(node_measure(.subset2(x, "prefix"), ".size"))
    p <- .ft_find_name_position_impl(.subset2(x, "middle"), target, offset)
    if(!is.na(p)) {
      return(p)
    }
    offset <- offset + as.integer(node_measure(.subset2(x, "middle"), ".size"))
    return(.ft_find_name_position_impl(.subset2(x, "suffix"), target, offset))
  }
  for(el in x) {
    p <- .ft_find_name_position_impl(el, target, offset)
    if(!is.na(p)) {
      return(p)
    }
    offset <- offset + .ft_child_size(el)
  }
  NA_integer_
}

# find one name position without constructing full name->position map.
# Runtime: O(n) worst-case.
.ft_find_name_position(t, target) %::% . : character : integer
.ft_find_name_position(t, target) %as% {
  .ft_find_name_position_impl(t, target, 0L)
}

# match requested names to positions. If strict_missing is FALSE, missing names
# are represented as NA integer placeholders.
# Runtime: O(n + m), where n=tree size and m=length(idx).
.ft_match_name_indices(t, idx, strict_missing) %::% . : character : logical : integer
.ft_match_name_indices(t, idx, strict_missing = FALSE) %as% {
  n <- as.integer(node_measure(t, ".size"))
  nn <- as.integer(node_measure(t, ".named_count"))
  if(n == 0L || nn == 0L) {
    stop("Tree has no element names.")
  }
  if(nn != n) {
    stop("Invalid name state: mixed named and unnamed elements are not allowed.")
  }
  if(length(idx) == 1L) {
    p <- if(.ft_cpp_enabled()) .ft_cpp_find_name_position(t, idx[[1]]) else .ft_find_name_position(t, idx[[1]])
    if(isTRUE(strict_missing) && is.na(p)) {
      stop("Unknown element name(s): ", idx[[1]])
    }
    return(as.integer(p))
  }

  if(length(idx) >= .ft_name_map_threshold()) {
    name_to_pos <- .ft_name_positions_fast(t)
    pos <- unname(name_to_pos[idx])
    if(isTRUE(strict_missing) && any(is.na(pos))) {
      missing_names <- unique(idx[is.na(pos)])
      stop("Unknown element name(s): ", paste(missing_names, collapse = ", "))
    }
    return(as.integer(pos))
  }

  pos <- integer(length(idx))
  for(k in seq_along(idx)) {
    p <- if(.ft_cpp_enabled()) .ft_cpp_find_name_position(t, idx[[k]]) else .ft_find_name_position(t, idx[[k]])
    if(isTRUE(strict_missing) && is.na(p)) {
      stop("Unknown element name(s): ", idx[[k]])
    }
    pos[[k]] <- p
  }
  as.integer(pos)
}

# internal positional read that preserves ft_name metadata.
# Runtime: O(log n) via indexed locate path (C++ or R fallback).
.ft_get_elem_at(x, idx) %::% . : integer : .
.ft_get_elem_at(x, idx) %as% {
  if(.ft_cpp_enabled()) {
    return(.ft_cpp_get_by_index(x, idx))
  }
  hit <- locate(x, function(v) v >= idx, ".size")
  if(!isTRUE(hit$found)) {
    stop("Index out of bounds.")
  }
  hit$elem
}

# bulk positional read helper preserving ft_name metadata.
# Runtime: O(m log n) fallback; C++ path is typically faster.
.ft_get_elems_at(x, idx) %::% . : integer : list
.ft_get_elems_at(x, idx) %as% {
  if(length(idx) == 0L) {
    return(list())
  }
  if(.ft_cpp_enabled()) {
    return(.ft_cpp_get_many_by_index(x, idx))
  }
  lapply(idx, function(i) .ft_get_elem_at(x, i))
}

# extract a name carried directly by a scalar replacement value.
# Runtime: O(1).
.ft_name_from_value(el) %::% . : .
.ft_name_from_value(el) %as% {
  nms <- names(el)
  if(is.null(nms) || length(nms) != 1L) {
    return(NULL)
  }
  .ft_normalize_name(nms[[1]])
}

# effective replacement name: explicit outer list name first, then existing
# internal name, then name carried by the replacement value itself.
# Runtime: O(1).
.ft_effective_name(el, explicit_name) %::% . : . : .
.ft_effective_name(el, explicit_name = NULL) %as% {
  nm <- .ft_normalize_name(explicit_name)
  if(is.null(nm)) {
    nm <- .ft_get_name(el)
  }
  if(is.null(nm)) {
    nm <- .ft_name_from_value(el)
  }
  nm
}

# update name mapping for sequential replacement and enforce uniqueness.
# Runtime: O(1) average map updates.
.ft_update_name_map(name_to_pos, name_vec, pos, nm) %::% . : character : integer : character : list
.ft_update_name_map(name_to_pos, name_vec, pos, nm) %as% {
  old_nm <- name_vec[[pos]]
  if(identical(nm, old_nm)) {
    return(list(name_to_pos = name_to_pos, name_vec = name_vec))
  }
  existing <- if(nm %in% names(name_to_pos)) as.integer(name_to_pos[[nm]]) else NULL
  if(!is.null(existing) && existing != pos) {
    stop("Element names must be unique.")
  }
  name_to_pos <- name_to_pos[names(name_to_pos) != old_nm]
  name_to_pos[[nm]] <- pos
  name_vec[[pos]] <- nm
  list(name_to_pos = name_to_pos, name_vec = name_vec)
}

#' Subset a flexseq by position or element name
#'
#' @method [ flexseq
#' @param x A `flexseq`.
#' @param i Positive integer indices, character element names, or logical mask.
#' @param ... Unused.
#' @return A new `flexseq` containing selected elements in query order.
#'   For character indexing, missing names are represented as `NULL` elements.
#' @examples
#' t <- as_flexseq(letters[1:6])
#' s <- t[c(2, 4, 6)]
#' cat_m <- measure_monoid(paste0, "", as.character)
#' fold_left(s, cat_m)
#'
#' # Empty index returns empty tree
#' attr(t[integer(0)], "measures")$.size
#'
#' # Character indexing by element names
#' tn <- as_flexseq(setNames(as.list(letters[1:4]), c("w", "x", "y", "z")))
#' out <- tn[c("y", "missing", "w")]
#' fold_left(out, measure_monoid(paste0, "", function(el) if(is.null(el)) "_" else el))
#'
#' # Logical indexing with recycling
#' t[c(TRUE, FALSE)]
#' @export
# Runtime: integer/logical reads O(m log n), where m = selected positions;
# character reads are adaptive: O(m * n_lookup) for short queries, O(n + m)
# with name-map path for wider queries.
`[.flexseq` <- function(x, i, ...) {
  if(missing(i)) {
    return(x)
  }
  ms <- resolve_tree_monoids(x, required = TRUE)
  n <- as.integer(node_measure(x, ".size"))

  if(is.logical(i)) {
    mask <- .ft_assert_lgl_indices(i, n)
    idx <- .ft_true_positions(mask)
    if(length(idx) == 0L) {
      return(empty_tree(monoids = ms))
    }
    out <- lapply(.ft_get_elems_at(x, idx), .ft_strip_name)
    return(tree_from(out, monoids = ms))
  }

  if(is.character(i)) {
    idx <- .ft_assert_chr_indices(i)
    if(length(idx) == 0L) {
      return(empty_tree(monoids = ms))
    }

    pos <- .ft_match_name_indices(x, idx, strict_missing = FALSE)
    out <- vector("list", length(pos))
    valid <- which(!is.na(pos))
    if(length(valid) > 0L) {
      vals <- .ft_get_elems_at(x, as.integer(pos[valid]))
      for(j in seq_along(valid)) {
        out[[valid[[j]]]] <- .ft_strip_name(vals[[j]])
      }
    }
    return(tree_from(out, monoids = ms))
  }

  idx <- .ft_assert_int_indices(i, n)
  if(length(idx) == 0L) {
    return(empty_tree(monoids = ms))
  }
  out <- lapply(.ft_get_elems_at(x, idx), .ft_strip_name)
  tree_from(out, monoids = ms)
}

#' Extract one element by position or unique name
#'
#' @method [[ flexseq
#' @param x A `flexseq`.
#' @param i Positive scalar integer index, or scalar character element name.
#' @param ... Unused.
#' @return The extracted element (internal name metadata is removed).
#' @examples
#' t <- as_flexseq(letters[1:5])
#' t[[3]]
#'
#' tn <- as_flexseq(setNames(as.list(letters[1:3]), c("a1", "a2", "a3")))
#' tn[["a2"]]
#' @export
# Runtime: O(log n) for integer lookup; O(n) for name lookup.
`[[.flexseq` <- function(x, i, ...) {
  if(is.character(i) && length(i) == 1L && !is.na(i)) {
    pos <- .ft_match_name_indices(x, i, strict_missing = TRUE)
    return(.ft_strip_name(.ft_get_elem_at(x, pos)))
  }
  ms <- resolve_tree_monoids(x, required = TRUE)
  n <- as.integer(node_measure(x, ".size"))
  idx <- .ft_assert_int_indices(i, n)
  if(length(idx) != 1L) {
    stop("[[ expects exactly one index.")
  }

  .ft_strip_name(.ft_get_elem_at(x, idx))
}

#' Replace selected elements by position or name
#'
#' @method [<- flexseq
#' @param x A `flexseq`.
#' @param i Positive integer indices, character names, or logical mask.
#' @param value Replacement values; must have exactly same length as `i`.
#' @return A new `flexseq` with selected elements replaced.
#' @examples
#' t <- as_flexseq(1:6)
#' t[c(2, 5)] <- list(20, 50)
#' sum_m <- measure_monoid(`+`, 0, as.numeric)
#' fold_left(t, sum_m)
#'
#' # Replacement length must match
#' try(t[c(1, 2)] <- list(999))
#'
#' # Character replacement by element names (missing names error)
#' tn <- as_flexseq(setNames(as.list(1:4), c("a", "b", "c", "d")))
#' tn[c("d", "a")] <- list(40, 10)
#'
#' # Logical replacement with recycling
#' t[c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)] <- list(1)
#' @export
# Runtime: sparse replacement path O(k log n); dense path O(n + k), where
# n = tree size and k = number of replaced positions.
`[<-.flexseq` <- function(x, i, value) {
  ms <- resolve_tree_monoids(x, required = TRUE)
  vals <- as.list(value)
  n <- as.integer(node_measure(x, ".size"))

  if(is.logical(i)) {
    mask <- .ft_assert_lgl_indices(i, n)
    idx <- .ft_true_positions(mask)
    vals2 <- .ft_recycle_values(vals, length(idx))
    return(`[<-.flexseq`(x, idx, vals2))
  }

  if(is.character(i)) {
    idx <- .ft_assert_chr_indices(i)
    if(length(vals) != length(idx)) {
      stop("Replacement length must match index length exactly.")
    }
    if(length(idx) == 0L) {
      return(x)
    }
    xs <- .ft_to_list(x)
    has_name_hints <- .ft_values_have_name_hints(vals)
    if(!has_name_hints) {
      pos <- .ft_match_name_indices(x, idx, strict_missing = TRUE)
      for(k in seq_along(pos)) {
        xs[pos[[k]]] <- list(.ft_set_name(vals[[k]], idx[[k]]))
      }
      return(tree_from(xs, monoids = ms))
    }

    name_to_pos <- .ft_name_positions_fast(x)
    pos <- unname(name_to_pos[idx])
    if(any(is.na(pos))) {
      missing_names <- unique(idx[is.na(pos)])
      stop("Unknown element name(s): ", paste(missing_names, collapse = ", "))
    }
    pos <- as.integer(pos)
    vn <- names(vals)
    name_vec <- names(name_to_pos)
    for(k in seq_along(pos)) {
      name_hint <- if(!is.null(vn)) vn[[k]] else NULL
      nm <- .ft_effective_name(vals[[k]], name_hint)
      if(is.null(nm)) {
        nm <- idx[[k]]
      }
      upd <- .ft_update_name_map(name_to_pos, name_vec, pos[[k]], nm)
      name_to_pos <- upd$name_to_pos
      name_vec <- upd$name_vec
      xs[pos[[k]]] <- list(.ft_set_name(vals[[k]], nm))
    }
    return(tree_from(xs, monoids = ms))
  }

  idx <- .ft_assert_int_indices(i, n)
  if(length(vals) != length(idx)) {
    stop("Replacement length must match index length exactly.")
  }
  vn <- names(vals)
  has_name_hints <- .ft_values_have_name_hints(vals)
  n_named <- as.integer(node_measure(x, ".named_count"))
  use_name_map <- n > 0L && n_named == n

  # Sparse vector replacement is usually faster as repeated point updates.
  # Dense replacement is faster as flatten+single rebuild.
  sparse_cutoff <- max(8L, as.integer(sqrt(max(1L, n))))
  if(length(idx) <= sparse_cutoff) {
    out <- x
    if(use_name_map && has_name_hints) {
      name_to_pos <- .ft_name_positions_fast(x)
      name_vec <- names(name_to_pos)
    }
    for(k in seq_along(idx)) {
      name_hint <- if(!is.null(vn)) vn[[k]] else NULL
      nm <- .ft_effective_name(vals[[k]], name_hint)
      if(is.null(nm)) {
        old <- .ft_get_elem_at(out, idx[[k]])
        nm <- .ft_get_name(old)
      }
      if(use_name_map && has_name_hints && !is.null(nm)) {
        upd <- .ft_update_name_map(name_to_pos, name_vec, idx[[k]], nm)
        name_to_pos <- upd$name_to_pos
        name_vec <- upd$name_vec
      }
      out[[idx[[k]]]] <- .ft_set_name(vals[[k]], nm)
    }
    return(out)
  }

  xs <- .ft_to_list(x)
  if(use_name_map && has_name_hints) {
    name_to_pos <- .ft_name_positions_fast(x)
    name_vec <- names(name_to_pos)
  }
  for(k in seq_along(idx)) {
    name_hint <- if(!is.null(vn)) vn[[k]] else NULL
    nm <- .ft_effective_name(vals[[k]], name_hint)
    if(is.null(nm)) {
      old <- xs[[idx[[k]]]]
      nm <- .ft_get_name(old)
    }
    if(use_name_map && has_name_hints && !is.null(nm)) {
      upd <- .ft_update_name_map(name_to_pos, name_vec, idx[[k]], nm)
      name_to_pos <- upd$name_to_pos
      name_vec <- upd$name_vec
    }
    v <- .ft_set_name(vals[[k]], nm)
    xs[idx[[k]]] <- list(v)
  }
  tree_from(xs, monoids = ms)
}

#' Replace one element by position or unique name
#'
#' @method [[<- flexseq
#' @param x A `flexseq`.
#' @param i Positive scalar integer index, or scalar character element name.
#' @param value Replacement element.
#' @return A new `flexseq` with one element replaced.
#' @examples
#' t <- as_flexseq(letters[1:4])
#' t[[2]] <- "ZZ"
#' cat_m <- measure_monoid(paste0, "", as.character)
#' fold_left(t, cat_m)
#'
#' tn <- as_flexseq(setNames(as.list(1:3), c("x", "y", "z")))
#' tn[["y"]] <- 99
#'
#' # Assigning NULL removes an element (by index or name)
#' t <- as_flexseq(letters[1:4])
#' t[[2]] <- NULL
#' tn <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
#' tn[["b"]] <- NULL
#' @export
# Runtime: O(n) via split + append + concat.
`[[<-.flexseq` <- function(x, i, value) {
  if(is.character(i) && length(i) == 1L && !is.na(i)) {
    pos <- .ft_match_name_indices(x, i, strict_missing = TRUE)
    if(is.null(value)) {
      return(`[[<-.flexseq`(x, pos, NULL))
    }
    nm <- .ft_effective_name(value)
    if(is.null(nm)) {
      nm <- i
    }
    v <- .ft_set_name(value, nm)
    return(`[[<-.flexseq`(x, pos, v))
  }
  ms <- resolve_tree_monoids(x, required = TRUE)
  n <- as.integer(node_measure(x, ".size"))
  idx <- .ft_assert_int_indices(i, n)
  if(length(idx) != 1L) {
    stop("[[<- expects exactly one index.")
  }
  if(is.null(value)) {
    s <- split_tree(x, function(v) v >= idx, ".size")
    return(.as_flexseq(concat(s$left, s$right, ms)))
  }
  old <- NULL
  nm <- .ft_effective_name(value)
  if(is.null(nm)) {
    old <- .ft_get_elem_at(x, idx)
    nm <- .ft_get_name(old)
  }
  n_named <- as.integer(node_measure(x, ".named_count"))
  if(n > 0L && n_named == n && !is.null(nm)) {
    if(is.null(old)) {
      old <- .ft_get_elem_at(x, idx)
    }
    old_nm <- .ft_get_name(old)
    if(is.null(old_nm)) {
      stop("Invalid name state: mixed named and unnamed elements are not allowed.")
    }
    if(!identical(nm, old_nm)) {
      existing <- if(.ft_cpp_enabled()) .ft_cpp_find_name_position(x, nm) else .ft_find_name_position(x, nm)
      if(!is.na(existing) && as.integer(existing) != as.integer(idx)) {
        stop("Element names must be unique.")
      }
    }
  }
  value <- .ft_set_name(value, nm)

  s <- split_tree(x, function(v) v >= idx, ".size")
  left_plus <- append(s$left, value)
  .as_flexseq(concat(left_plus, s$right, ms))
}
