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
tree_from <- function(x, values = NULL, monoids = NULL) {
  ms <- if(is.null(monoids)) {
    list(.size = size_measure_monoid())
  } else {
    ensure_size_monoids(monoids)
  }

  t <- empty_tree(monoids = ms)
  x_list <- as.list(x)
  in_names <- names(x)
  use_names <- !is.null(in_names) && length(in_names) > 0L
  if(use_names && length(in_names) != length(x_list)) {
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
      in_names <- norm_names
    } else {
      in_names <- NULL
    }
  }

  if(is.null(values)) {
    for(i in seq_along(x_list)) {
      el <- x_list[[i]]
      if(!is.null(in_names)) {
        el <- .ft_set_name(el, in_names[[i]])
      }
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
      if(!is.null(in_names)) {
        el <- .ft_set_name(el, in_names[[i]])
      }
      t <- append(t, el)
    }
  }

  .ft_assert_name_state(t)
  assert_structural_attrs(t)
  t
}
