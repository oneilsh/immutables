resolve_tree_monoids <- function(t, required = FALSE) {
  ms <- attr(t, "monoids", exact = TRUE)
  if(required && is.null(ms)) {
    stop("Tree has no monoids attribute.")
  }
  if(is.null(ms)) {
    return(NULL)
  }
  ensure_size_monoids(ms)
}

resolve_named_monoid <- function(t, monoid_name) {
  if(!is.character(monoid_name) || length(monoid_name) != 1L || is.na(monoid_name) || monoid_name == "") {
    stop("`monoid_name` must be a single non-empty string.")
  }
  ms <- resolve_tree_monoids(t, required = TRUE)
  mr <- ms[[monoid_name]]
  if(is.null(mr)) {
    stop(paste0("Monoid name `", monoid_name, "` not found in tree monoids."))
  }
  list(monoids = ms, monoid = mr)
}

merge_monoid_sets <- function(base, add, overwrite = FALSE) {
  b <- ensure_size_monoids(base)
  a <- ensure_size_monoids(add)

  overlap <- intersect(names(b), names(a))
  overlap <- setdiff(overlap, ".size")
  if(length(overlap) > 0 && !isTRUE(overwrite)) {
    stop("Monoid names already exist: ", paste(overlap, collapse = ", "), ". Use overwrite = TRUE to replace.")
  }

  if(length(overlap) > 0 && isTRUE(overwrite)) {
    for(nm in overlap) {
      b[[nm]] <- a[[nm]]
    }
  }

  add_only <- setdiff(names(a), names(b))
  if(length(add_only) > 0) {
    b <- c(b, a[add_only])
  }
  ensure_size_monoids(b)
}

emit_concat_assumption_warning <- function(shared_names) {
  msg <- paste0(
    "concat_trees(): shared monoid names assumed equivalent; left-tree definitions are used for: ",
    paste(shared_names, collapse = ", ")
  )
  w <- structure(
    list(message = msg, call = NULL),
    class = c("fingertree_monoid_assumption_warning", "warning", "condition")
  )
  warning(w)
}

#' Create an empty finger tree
#'
#' @param monoids Optional named list of `MeasureMonoid` objects.
#' @return An empty finger tree with structural `monoids` and `measures` attrs.
#' @export
empty_tree <- function(monoids = NULL) {
  ms <- if(is.null(monoids)) {
    list(.size = size_measure_monoid())
  } else {
    ensure_size_monoids(monoids)
  }
  t <- measured_empty(ms)
  assert_structural_attrs(t)
  t
}

#' Build a tree from a vector or list
#'
#' @param x Elements to insert.
#' @param values Optional parallel values (same length as `x`).
#' @param monoids Optional named list of `MeasureMonoid` objects.
#' @return A finger tree with cached measures for all monoids.
#' @export
tree_from <- function(x, values = NULL, monoids = NULL) {
  ms <- if(is.null(monoids)) {
    list(.size = size_measure_monoid())
  } else {
    ensure_size_monoids(monoids)
  }

  t <- empty_tree(monoids = ms)
  x_list <- as.list(x)

  if(is.null(values)) {
    for(el in x_list) {
      t <- append(t, el)
    }
  } else {
    v_list <- as.list(values)
    if(length(x_list) != length(v_list)) {
      stop("length of entries and values lists given to tree_from not equal.")
    }
    for(i in seq_along(x_list)) {
      el <- x_list[[i]]
      attr(el, "value") <- v_list[[i]]
      t <- append(t, el)
    }
  }

  assert_structural_attrs(t)
  t
}

#' Add/merge monoids on an existing tree
#'
#' @param t FingerTree.
#' @param monoids Named list of `MeasureMonoid` objects to add.
#' @param overwrite Logical; whether overlapping names replace existing monoids.
#' @return A persistent copy with recomputed cached measures.
#' @export
add_monoids <- function(t, monoids, overwrite = FALSE) {
  assert_structural_attrs(t)
  add <- ensure_size_monoids(monoids)
  cur <- resolve_tree_monoids(t, required = TRUE)
  merged <- merge_monoid_sets(cur, add, overwrite = overwrite)

  add_only <- setdiff(names(add), names(cur))
  overlap <- setdiff(intersect(names(add), names(cur)), ".size")
  recompute_names <- add_only
  if(isTRUE(overwrite) && length(overlap) > 0) {
    recompute_names <- union(recompute_names, overlap)
  }
  recompute_names <- setdiff(recompute_names, ".size")

  if(length(recompute_names) == 0) {
    return(t)
  }

  t2 <- rebind_tree_monoids(t, merged, recompute_names)
  assert_structural_attrs(t2)
  t2
}

#' Prepend an element
#'
#' @param t FingerTree.
#' @param x Element to prepend.
#' @return Updated tree.
#' @export
prepend <- function(t, x) {
  assert_structural_attrs(t)
  ms <- resolve_tree_monoids(t, required = TRUE)
  t2 <- add_left(t, x, ms)
  assert_structural_attrs(t2)
  t2
}

#' Append an element
#'
#' @param t FingerTree.
#' @param x Element to append.
#' @return Updated tree.
#' @export
append <- function(t, x) {
  assert_structural_attrs(t)
  ms <- resolve_tree_monoids(t, required = TRUE)
  t2 <- add_right(t, x, ms)
  assert_structural_attrs(t2)
  t2
}

#' Concatenate two trees
#'
#' Same-name monoids are assumed equivalent; left-tree definitions win.
#' Missing monoids are added to each side before concatenation.
#'
#' @param x Left tree.
#' @param y Right tree.
#' @return Concatenated tree.
#' @export
concat_trees <- function(x, y) {
  assert_structural_attrs(x)
  assert_structural_attrs(y)

  mx <- resolve_tree_monoids(x, required = TRUE)
  my <- resolve_tree_monoids(y, required = TRUE)

  shared <- intersect(names(mx), names(my))
  shared <- setdiff(shared, ".size")
  if(length(shared) > 0) {
    emit_concat_assumption_warning(shared)
  }

  left_only <- setdiff(names(mx), names(my))
  left_only <- setdiff(left_only, ".size")
  right_only <- setdiff(names(my), names(mx))
  right_only <- setdiff(right_only, ".size")

  x2 <- if(length(right_only) > 0) add_monoids(x, my[right_only], overwrite = FALSE) else x
  y2 <- if(length(left_only) > 0) add_monoids(y, mx[left_only], overwrite = FALSE) else y

  merged <- c(mx, my[setdiff(names(my), names(mx))])
  merged <- ensure_size_monoids(merged)

  t <- concat(x2, y2, merged)
  assert_structural_attrs(t)
  t
}

#' Reduce from the left
#'
#' @param t FingerTree.
#' @param monoid A `MeasureMonoid` object.
#' @return Reduced value.
#' @export
reduce_left <- function(t, monoid) {
  if(!is_measure_monoid(monoid)) {
    stop("`monoid` must be a MeasureMonoid object.")
  }
  reduce_left_impl(t, monoid)
}

#' Reduce from the right
#'
#' @param t FingerTree.
#' @param monoid A `MeasureMonoid` object.
#' @return Reduced value.
#' @export
reduce_right <- function(t, monoid) {
  if(!is_measure_monoid(monoid)) {
    stop("`monoid` must be a MeasureMonoid object.")
  }
  reduce_right_impl(t, monoid)
}

#' Split tree around first predicate flip
#'
#' @param t FingerTree.
#' @param predicate Function on measure values.
#' @param monoid_name Name of monoid from `attr(t, "monoids")`.
#' @param accumulator Optional starting measure (defaults to monoid identity).
#' @return A list with `left`, `elem`, and `right`.
#' @export
split_tree <- function(t, predicate, monoid_name, accumulator = NULL) {
  assert_structural_attrs(t)
  ctx <- resolve_named_monoid(t, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid

  if(t %isa% Empty) {
    stop("split_tree requires a non-empty tree.")
  }

  i <- if(is.null(accumulator)) mr$i else accumulator
  res <- split_tree_impl(predicate, i, t, ms, monoid_name)
  assert_structural_attrs(res$left)
  assert_structural_attrs(res$right)
  res
}

#' Split tree into left and right parts
#'
#' @param t FingerTree.
#' @param predicate Function on measure values.
#' @param monoid_name Name of monoid from `attr(t, "monoids")`.
#' @return A list with `left` and `right`.
#' @export
split <- function(t, predicate, monoid_name) {
  assert_structural_attrs(t)
  ctx <- resolve_named_monoid(t, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid

  if(t %isa% Empty) {
    out <- list(left = measured_empty(ms), right = measured_empty(ms))
    assert_structural_attrs(out$left)
    assert_structural_attrs(out$right)
    return(out)
  }

  if(predicate(node_measure(t, monoid_name))) {
    s <- split_tree_impl(predicate, mr$i, t, ms, monoid_name)
    right <- prepend(s$right, s$elem)
    out <- list(left = s$left, right = right)
    assert_structural_attrs(out$left)
    assert_structural_attrs(out$right)
    return(out)
  }

  out <- list(left = t, right = measured_empty(ms))
  assert_structural_attrs(out$left)
  assert_structural_attrs(out$right)
  out
}
