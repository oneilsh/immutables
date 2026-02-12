# flatten a fingertree into its element sequence (left-to-right)
# Runtime: O(n) in number of elements.
.ft_to_list(x) %::% . : list
.ft_to_list(x) %as% {
  if(!is_structural_node(x)) {
    return(list(x))
  }
  if(x %isa% Empty) {
    return(list())
  }
  if(x %isa% Single) {
    return(.ft_to_list(.subset2(x, 1)))
  }
  if(x %isa% Deep) {
    return(c(.ft_to_list(.subset2(x,"prefix")), .ft_to_list(.subset2(x,"middle")), .ft_to_list(.subset2(x,"suffix"))))
  }
  out <- list()
  for(el in x) {
    out <- c(out, .ft_to_list(el))
  }
  out
}

# Runtime: O(n) worst-case in relevant input/subtree size.
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

# Runtime: O(n) worst-case in relevant input/subtree size.
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

# Runtime: O(n) worst-case in relevant input/subtree size.
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

# Runtime: O(n) worst-case in relevant input/subtree size.
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

# remove internal naming metadata from user-visible element returns.
# Runtime: O(n) worst-case in relevant input/subtree size.
.ft_strip_name(el) %::% . : .
.ft_strip_name(el) %as% {
  attr(el, "ft_name") <- NULL
  el
}

# collect internal element names in left-to-right order.
# Runtime: O(n) (calls `.ft_to_list`).
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
  .ft_assert_name_state(t)
  n <- as.integer(node_measure(t, ".size"))
  nn <- as.integer(node_measure(t, ".named_count"))
  if(n == 0L || nn == 0L) {
    stop("Tree has no element names.")
  }
  nms <- .ft_collect_names(t)
  pos <- seq_len(length(nms))
  names(pos) <- nms
  pos
}

# match requested names to positions. If strict_missing is FALSE, missing names
# are represented as NA integer placeholders.
# Runtime: O(n + m), where n=tree size and m=length(idx).
.ft_match_name_indices(t, idx, strict_missing) %::% . : character : logical : integer
.ft_match_name_indices(t, idx, strict_missing = FALSE) %as% {
  name_to_pos <- .ft_name_positions(t)
  pos <- unname(name_to_pos[idx])
  if(isTRUE(strict_missing) && any(is.na(pos))) {
    missing_names <- unique(idx[is.na(pos)])
    stop("Unknown element name(s): ", paste(missing_names, collapse = ", "))
  }
  as.integer(pos)
}

# internal positional read that preserves ft_name metadata.
# Runtime: O(n) worst-case in relevant input/subtree size.
.ft_get_elem_at(x, idx) %::% . : integer : .
.ft_get_elem_at(x, idx) %as% {
  hit <- locate(x, function(v) v >= idx, ".size")
  if(!isTRUE(hit$found)) {
    stop("Index out of bounds.")
  }
  hit$elem
}

# extract a name carried directly by a scalar replacement value.
# Runtime: O(n) worst-case in relevant input/subtree size.
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
# Runtime: O(n) worst-case in relevant input/subtree size.
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

#' Subset a finger tree by position or element name
#'
#' @param x FingerTree.
#' @param i Positive integer indices, character element names, or logical mask.
#' @param ... Unused.
#' @return A new FingerTree containing selected elements in query order.
#'   For character indexing, missing names are represented as `NULL` elements.
#' @examples
#' t <- tree_from(letters[1:6])
#' s <- t[c(2, 4, 6)]
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_left(s, cat_m)
#'
#' # Empty index returns empty tree
#' attr(t[integer(0)], "measures")$.size
#'
#' # Character indexing by element names
#' tn <- tree_from(setNames(as.list(letters[1:4]), c("w", "x", "y", "z")))
#' out <- tn[c("y", "missing", "w")]
#' reduce_left(out, MeasureMonoid(paste0, "", function(el) if(is.null(el)) "_" else el))
#'
#' # Logical indexing with recycling
#' t[c(TRUE, FALSE)]
#' @export
# Runtime: O(n) for logical/character paths; O(m log n) for integer path
# where m = length(i) and n = tree size.
`[.FingerTree` <- function(x, i, ...) {
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
    out <- lapply(idx, function(j) {
      .ft_strip_name(.ft_get_elem_at(x, j))
    })
    return(tree_from(out, monoids = ms))
  }

  if(is.character(i)) {
    idx <- .ft_assert_chr_indices(i)
    if(length(idx) == 0L) {
      return(empty_tree(monoids = ms))
    }

    pos <- .ft_match_name_indices(x, idx, strict_missing = FALSE)
    out <- vector("list", length(pos))
    for(k in seq_along(pos)) {
      p <- pos[[k]]
      if(is.na(p)) {
        out[[k]] <- NULL
      } else {
        out[[k]] <- .ft_strip_name(.ft_get_elem_at(x, p))
      }
    }
    return(tree_from(out, monoids = ms))
  }

  idx <- .ft_assert_int_indices(i, n)
  if(length(idx) == 0L) {
    return(empty_tree(monoids = ms))
  }
  out <- lapply(idx, function(j) {
    .ft_strip_name(.ft_get_elem_at(x, j))
  })
  tree_from(out, monoids = ms)
}

#' Extract one element by position or unique name
#'
#' @param x FingerTree.
#' @param i Positive scalar integer index, or scalar character element name.
#' @param ... Unused.
#' @return The extracted element (internal name metadata is removed).
#' @examples
#' t <- tree_from(letters[1:5])
#' t[[3]]
#'
#' tn <- tree_from(setNames(as.list(letters[1:3]), c("a1", "a2", "a3")))
#' tn[["a2"]]
#' @export
# Runtime: O(log n) for integer lookup; O(n) for name lookup.
`[[.FingerTree` <- function(x, i, ...) {
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
#' @param x FingerTree.
#' @param i Positive integer indices, character names, or logical mask.
#' @param value Replacement values; must have exactly same length as `i`.
#' @return A new FingerTree with selected elements replaced.
#' @examples
#' t <- tree_from(1:6)
#' t[c(2, 5)] <- list(20, 50)
#' sum_m <- MeasureMonoid(`+`, 0, as.numeric)
#' reduce_left(t, sum_m)
#'
#' # Replacement length must match
#' try(t[c(1, 2)] <- list(999))
#'
#' # Character replacement by element names (missing names error)
#' tn <- tree_from(setNames(as.list(1:4), c("a", "b", "c", "d")))
#' tn[c("d", "a")] <- list(40, 10)
#'
#' # Logical replacement with recycling
#' t[c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)] <- list(1)
#' @export
# Runtime: O(k * n) worst-case due sequential single-element replacements,
# where k = number of replaced positions.
`[<-.FingerTree` <- function(x, i, value) {
  resolve_tree_monoids(x, required = TRUE)
  vals <- as.list(value)
  n <- as.integer(node_measure(x, ".size"))

  if(is.logical(i)) {
    mask <- .ft_assert_lgl_indices(i, n)
    idx <- .ft_true_positions(mask)
    vals2 <- .ft_recycle_values(vals, length(idx))
    return(`[<-.FingerTree`(x, idx, vals2))
  }

  if(is.character(i)) {
    idx <- .ft_assert_chr_indices(i)
    if(length(vals) != length(idx)) {
      stop("Replacement length must match index length exactly.")
    }
    if(length(idx) == 0L) {
      return(x)
    }
    name_to_pos <- .ft_name_positions(x)
    pos <- unname(name_to_pos[idx])
    if(any(is.na(pos))) {
      missing_names <- unique(idx[is.na(pos)])
      stop("Unknown element name(s): ", paste(missing_names, collapse = ", "))
    }
    pos <- as.integer(pos)
    out <- x
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
      v <- .ft_set_name(vals[[k]], nm)
      out[[pos[[k]]]] <- v
    }
    return(out)
  }

  idx <- .ft_assert_int_indices(i, n)
  if(length(vals) != length(idx)) {
    stop("Replacement length must match index length exactly.")
  }
  out <- x
  vn <- names(vals)
  n_named <- as.integer(node_measure(x, ".named_count"))
  use_name_map <- n > 0L && n_named == n
  if(use_name_map) {
    name_to_pos <- .ft_name_positions(x)
    name_vec <- names(name_to_pos)
  }
  for(k in seq_along(idx)) {
    name_hint <- if(!is.null(vn)) vn[[k]] else NULL
    nm <- .ft_effective_name(vals[[k]], name_hint)
    if(is.null(nm)) {
      old <- .ft_get_elem_at(out, idx[[k]])
      nm <- .ft_get_name(old)
    }
    if(use_name_map && !is.null(nm)) {
      upd <- .ft_update_name_map(name_to_pos, name_vec, idx[[k]], nm)
      name_to_pos <- upd$name_to_pos
      name_vec <- upd$name_vec
    }
    v <- .ft_set_name(vals[[k]], nm)
    out[[idx[[k]]]] <- v
  }
  out
}

#' Replace one element by position or unique name
#'
#' @param x FingerTree.
#' @param i Positive scalar integer index, or scalar character element name.
#' @param value Replacement element.
#' @return A new FingerTree with one element replaced.
#' @examples
#' t <- tree_from(letters[1:4])
#' t[[2]] <- "ZZ"
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' reduce_left(t, cat_m)
#'
#' tn <- tree_from(setNames(as.list(1:3), c("x", "y", "z")))
#' tn[["y"]] <- 99
#' @export
# Runtime: O(n) via split + append + concat.
`[[<-.FingerTree` <- function(x, i, value) {
  if(is.character(i) && length(i) == 1L && !is.na(i)) {
    pos <- .ft_match_name_indices(x, i, strict_missing = TRUE)
    nm <- .ft_effective_name(value)
    if(is.null(nm)) {
      nm <- i
    }
    v <- .ft_set_name(value, nm)
    return(`[[<-.FingerTree`(x, pos, v))
  }
  ms <- resolve_tree_monoids(x, required = TRUE)
  n <- as.integer(node_measure(x, ".size"))
  idx <- .ft_assert_int_indices(i, n)
  if(length(idx) != 1L) {
    stop("[[<- expects exactly one index.")
  }
  nm <- .ft_effective_name(value)
  if(is.null(nm)) {
    old <- .ft_get_elem_at(x, idx)
    nm <- .ft_get_name(old)
  }
  n_named <- as.integer(node_measure(x, ".named_count"))
  if(n > 0L && n_named == n && !is.null(nm)) {
    name_to_pos <- .ft_name_positions(x)
    name_vec <- names(name_to_pos)
    .ft_update_name_map(name_to_pos, name_vec, idx, nm)
  }
  value <- .ft_set_name(value, nm)

  s <- split_tree(x, function(v) v >= idx, ".size")
  left_plus <- append(s$left, value)
  concat(left_plus, s$right, ms)
}
