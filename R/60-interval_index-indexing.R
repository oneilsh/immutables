#' Indexing for Interval Indexes
#'
#' Read indexing returns `interval_index` subsets while preserving interval/key
#' order. Replacement indexing is blocked.
#'
#' @name sub-.interval_index
#' @param x An `interval_index`.
#' @param i Index input.
#' @param value Replacement value (unsupported).
#' @param ... Unused.
#' @return Read methods return interval payload values/subsets; replacement forms
#'   always error.
NULL

# Runtime: O(k log n) for reads + O(k) strict-order validation.
#' @rdname sub-.interval_index
#' @method [ interval_index
#' @export
`[.interval_index` <- function(x, i, ...) {
  if(missing(i)) {
    return(x)
  }
  .ivx_assert_index(x)

  ms <- resolve_tree_monoids(x, required = TRUE)
  n <- as.integer(node_measure(x, ".size"))

  if(is.logical(i)) {
    mask <- .ft_assert_lgl_indices(i, n)
    idx <- .ft_true_positions(mask)
    if(length(idx) == 0L) {
      return(.ivx_wrap_like(x, empty_tree(monoids = ms)))
    }
    out <- lapply(.ft_get_elems_at(x, idx), .ft_strip_name)
    return(.ivx_wrap_like(x, tree_from(out, monoids = ms)))
  }

  if(is.character(i)) {
    idx <- .ft_assert_chr_indices(i)
    if(length(idx) == 0L) {
      return(.ivx_wrap_like(x, empty_tree(monoids = ms)))
    }
    pos <- .ft_match_name_indices(x, idx, strict_missing = TRUE)
    pos <- .ord_assert_positions_strict(pos)
    out <- lapply(.ft_get_elems_at(x, pos), .ft_strip_name)
    return(.ivx_wrap_like(x, tree_from(out, monoids = ms)))
  }

  idx <- .ft_assert_int_indices(i, n)
  if(length(idx) == 0L) {
    return(.ivx_wrap_like(x, empty_tree(monoids = ms)))
  }
  idx <- .ord_assert_positions_strict(idx)
  out <- lapply(.ft_get_elems_at(x, idx), .ft_strip_name)
  .ivx_wrap_like(x, tree_from(out, monoids = ms))
}

# Runtime: O(log n) by index, O(n_lookup) by name.
#' @rdname sub-.interval_index
#' @method [[ interval_index
#' @export
`[[.interval_index` <- function(x, i, ...) {
  .ivx_assert_index(x)
  entry <- `[[.flexseq`(x, i, ...)
  if(!is.list(entry) || !("item" %in% names(entry))) {
    stop("Malformed interval_index entry.")
  }
  entry$item
}

# Runtime: O(1).
#' @rdname sub-.interval_index
#' @method [<- interval_index
#' @export
`[<-.interval_index` <- function(x, i, value) {
  stop("`[<-` is not supported for interval_index.")
}

# Runtime: O(1).
#' @rdname sub-.interval_index
#' @method [[<- interval_index
#' @export
`[[<-.interval_index` <- function(x, i, value) {
  stop("`[[<-` is not supported for interval_index.")
}

#' @rdname sub-.interval_index
#' @method $ interval_index
#' @param name Element name (for `$` and `$<-`).
#' @return For `$`: the matched payload element.
#' @export
`$.interval_index` <- function(x, name) {
  nm <- .ft_dollar_name(substitute(name))
  `[[.interval_index`(x, nm)
}

#' @rdname sub-.interval_index
#' @method $<- interval_index
#' @param value Replacement value (unsupported).
#' @return No return value; always errors because replacement indexing is unsupported.
#' @export
`$<-.interval_index` <- function(x, name, value) {
  stop("`$<-` is not supported for interval_index.")
}
