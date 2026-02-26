.ivx_insert_entry <- function(x, entry, endpoint_type = NULL) {
  ep <- if(is.null(endpoint_type)) .ivx_endpoint_type_state(x) else endpoint_type
  ms <- resolve_tree_monoids(x, required = TRUE)
  can_use_cpp_key_insert <- isTRUE(
    .ivx_supports_oms_key_type(ep) &&
      .ft_cpp_can_use(ms) &&
      !is.null(ms[[".oms_max_key"]])
  )

  out <- if(length(x) == 0L) {
    if(.ivx_supports_oms_key_type(ep)) {
      ms0 <- .ivx_merge_monoids(.ivx_user_monoids(x), endpoint_type = ep)
      if(.ft_cpp_can_use(ms0) && !is.null(ms0[[".oms_max_key"]])) {
        .ft_cpp_oms_insert(empty_tree(monoids = ms0), entry, ms0, ep)
      } else {
        .ft_push_back_impl(empty_tree(monoids = ms0), entry, context = "insert()")
      }
    } else {
      .ft_push_back_impl(x, entry, context = "insert()")
    }
  } else if(can_use_cpp_key_insert) {
    .ft_cpp_oms_insert(x, entry, ms, ep)
  } else {
    s <- split_by_predicate(
      x,
      function(v) isTRUE(v$has) && .ivx_compare_scalar(v$start, entry$start, v$endpoint_type) > 0L,
      ".ivx_max_start"
    )
    left_plus <- .ft_push_back_impl(s$left, entry, context = "insert()")
    concat_trees(left_plus, s$right)
  }

  .ivx_wrap_like(x, out, endpoint_type = ep)
}

# Runtime: O(log n) near split point depth.
#' Insert an element into an interval index
#'
#' @method insert interval_index
#' @param x An `interval_index`.
#' @param element Element to insert.
#' @param start Scalar start endpoint.
#' @param end Scalar end endpoint.
#' @param name Optional element name.
#' @param ... Unused.
#' @return Updated `interval_index`.
#' @export
insert.interval_index <- function(x, element, start, end, name = NULL, ...) {
  .ivx_assert_index(x)

  norm <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))
  entry <- .ivx_make_entry(element, norm$start, norm$end)

  if(!is.null(name)) {
    entry <- .ft_set_name(entry, name)
  }

  .ivx_insert_entry(x, entry, endpoint_type = norm$endpoint_type)
}
