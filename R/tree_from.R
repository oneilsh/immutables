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
