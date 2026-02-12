#' Build a tree from a vector or list
#'
#' @param x Elements to insert.
#' @param values Optional parallel values (same length as `x`).
#' @param monoids Optional named list of `MeasureMonoid` objects.
#' @return A finger tree with cached measures for all monoids.
#'   If `x` has names, they are used for name-based indexing and must be
#'   complete (no missing/empty names) and unique.
#' @examples
#' t <- tree_from(letters[1:4])
#' t[[2]]
#'
#' sum_m <- MeasureMonoid(`+`, 0, as.numeric)
#' t2 <- tree_from(1:5, monoids = list(sum = sum_m))
#' attr(t2, "measures")
#'
#' # Named elements support character indexing
#' tn <- tree_from(setNames(as.list(letters[1:3]), c("k1", "k2", "k3")))
#' tn[["k2"]]
#' @export
# Runtime: O(n log n) by repeated right-add without full-tree validation scans.
tree_from <- function(x, values = NULL, monoids = NULL) {
  ms <- if(is.null(monoids)) ensure_size_monoids(list(.size = size_measure_monoid())) else ensure_size_monoids(monoids)

  x_list <- as.list(x)
  n <- length(x_list)
  v_list <- NULL
  if(!is.null(values)) {
    v_list <- as.list(values)
    if(n != length(v_list)) {
      stop("length of entries and values lists given to tree_from not equal.")
    }
  }

  resolved_names <- NULL
  in_names <- names(x)
  use_names <- !is.null(in_names) && length(in_names) > 0L
  if(use_names && length(in_names) != n) {
    stop("Input names length must match input length.")
  }

  if(use_names) {
    norm_list <- lapply(in_names, .ft_normalize_name)
    has_any <- any(vapply(norm_list, function(nm) !is.null(nm), logical(1)))
    if(has_any) {
      if(any(vapply(norm_list, is.null, logical(1)))) {
        stop("Mixed named and unnamed elements are not allowed.")
      }
      norm_names <- unlist(norm_list, use.names = FALSE)
      if(anyDuplicated(norm_names) > 0L) {
        stop("Element names must be unique.")
      }
      resolved_names <- norm_names
    }
  } else if(n > 0L) {
    # Preserve existing behavior: when outer names are absent, derive names
    # from each element payload (ft_name attr or inline scalar names()).
    # Fast local path avoids lambda dispatch in hot construction workloads.
    derived <- vector("list", n)
    has_any <- FALSE
    has_missing <- FALSE
    for(i in seq_len(n)) {
      el <- x_list[[i]]
      nm <- attr(el, "ft_name", exact = TRUE)
      if(!is.null(nm) && length(nm) > 0L) {
        if(length(nm) != 1L) {
          stop("Element names must be scalar.")
        }
        nm <- as.character(nm[[1L]])
        if(is.na(nm) || nm == "") {
          nm <- NULL
        }
      } else {
        nms <- names(el)
        if(is.null(nms) || length(nms) != 1L) {
          nm <- NULL
        } else {
          nm <- nms[[1L]]
          if(is.na(nm) || nm == "") {
            nm <- NULL
          } else {
            nm <- as.character(nm)
          }
        }
      }
      derived[[i]] <- nm
      if(is.null(nm)) {
        has_missing <- TRUE
      } else {
        has_any <- TRUE
      }
    }
    if(has_any) {
      if(has_missing) {
        stop("Mixed named and unnamed elements are not allowed.")
      }
      resolved_names <- unlist(derived, use.names = FALSE)
      if(anyDuplicated(resolved_names) > 0L) {
        stop("Element names must be unique.")
      }
    }
  }

  if(.ft_cpp_can_use(ms)) {
    if(is.null(v_list) && is.null(resolved_names)) {
      return(.ft_cpp_tree_from(x_list, ms))
    }
    return(.ft_cpp_tree_from_prepared(
      x_list,
      if(is.null(v_list)) NULL else v_list,
      if(is.null(resolved_names)) NULL else resolved_names,
      ms
    ))
  }

  if(is.null(v_list) && is.null(resolved_names)) {
    t <- empty_tree(monoids = ms)
    for(i in seq_along(x_list)) {
      t <- .add_right_fast(t, x_list[[i]], ms)
    }
    return(t)
  }

  if(!is.null(v_list) || !is.null(resolved_names)) {
    for(i in seq_along(x_list)) {
      el <- x_list[[i]]
      if(!is.null(v_list)) {
        attr(el, "value") <- v_list[[i]]
      }
      if(!is.null(resolved_names)) {
        el <- .ft_set_name(el, resolved_names[[i]])
      }
      # Preserve NULL elements; `[[<- NULL` would drop positions from the list.
      x_list[i] <- list(el)
    }
  }

  t <- empty_tree(monoids = ms)
  for(i in seq_along(x_list)) {
    t <- .add_right_fast(t, x_list[[i]], ms)
  }
  t
}
