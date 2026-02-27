#SO

# Runtime: O(n).
# Returns per-entry interval endpoints in current sequence order.
# **Inputs:** `x` interval_index.
# **Outputs:** data.frame with list-cols `start` and `end`.
# **Used by:** users/tests.
#' Get interval bounds in sequence order
#'
#' @param x An `interval_index`.
#' @return A data frame in current sequence order with one row per entry and
#'   two list-columns:
#'   \describe{
#'     \item{`start`}{Start endpoint for each entry.}
#'     \item{`end`}{End endpoint for each entry.}
#'   }
#'   Returns a zero-row data frame with the same columns for empty indexes.
#' @export
interval_bounds <- function(x) {
  .ivx_assert_index(x)
  entries <- .ivx_entries(x)
  n <- length(entries)

  if(n == 0L) {
    return(data.frame(start = I(list()), end = I(list()), row.names = integer(0)))
  }

  starts <- unname(lapply(entries, function(e) e$start))
  ends <- unname(lapply(entries, function(e) e$end))
  data.frame(start = I(starts), end = I(ends), row.names = seq_len(n))
}

# Runtime: O(log n + c + k), where c = candidate count and k = matched count (worst-case O(n)).
# Point query (peek) endpoint.
# **Inputs:** `x` interval_index; scalar `point`; `which` in {"first","all"}; optional `bounds`.
# **Outputs:** payload item (first) or interval_index slice (all).
# **Used by:** users/tests.
#' Peek intervals containing a point
#'
#' @param x An `interval_index`.
#' @param point Query point.
#' @param which One of `"first"` or `"all"`.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return For `which = "first"`, the payload item from the first match (or
#'   `NULL` on no match). For `which = "all"`, an `interval_index` slice
#'   of matches.
#' @export
peek_point <- function(x, point, which = c("first", "all"), bounds = NULL) {
  .ivx_assert_index(x)
  peek_which <- match.arg(which)
  b <- .ivx_resolve_bounds(x, bounds)
  qp <- .ivx_normalize_endpoint(point, "point", endpoint_type = .ivx_endpoint_type_state(x))
  spec <- .ivx_spec_point(qp, b, .ivx_bounds_flags(b))
  .ivx_run_relation_query(x, spec, mode = "peek", which = peek_which)
}

# Runtime: O(log n + c) for `which = "first"`; O(n log n) for `which = "all"`.
# Point query (pop) endpoint.
# **Inputs:** `x` interval_index; scalar `point`; `which`; optional `bounds`.
# **Outputs:** pop result list with `element/start/end/remaining`.
# **Used by:** users/tests.
#' Pop intervals containing a point
#'
#' @param x An `interval_index`.
#' @param point Query point.
#' @param which One of `"first"` or `"all"`.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return For `which = "first"`, a list with `element`, `start`, `end`, `remaining`.
#'   For `which = "all"`, `element` is an `interval_index` slice and
#'   `start`/`end` are `NULL`.
#' @export
pop_point <- function(x, point, which = c("first", "all"), bounds = NULL) {
  .ivx_assert_index(x)
  pop_which <- match.arg(which)
  b <- .ivx_resolve_bounds(x, bounds)
  qp <- .ivx_normalize_endpoint(point, "point", endpoint_type = .ivx_endpoint_type_state(x))
  spec <- .ivx_spec_point(qp, b, .ivx_bounds_flags(b))
  .ivx_run_relation_query(x, spec, mode = "pop", which = pop_which)
}

# Runtime: O(log n + c + k), where c = candidate count and k = matched count (worst-case O(n)).
# Overlap relation query (peek) endpoint.
# **Inputs:** `x`; scalar `start`/`end`; `which`; optional `bounds`.
# **Outputs:** payload item (first) or interval_index slice (all).
# **Used by:** users/tests.
#' Peek intervals overlapping a query interval
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param which One of `"first"` or `"all"`.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return For `which = "first"`, the payload item from the first match (or
#'   `NULL` on no match). For `which = "all"`, an `interval_index` slice
#'   of matches.
#' @export
peek_overlaps <- function(x, start, end, which = c("first", "all"), bounds = NULL) {
  .ivx_assert_index(x)
  peek_which <- match.arg(which)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))
  spec <- .ivx_spec_overlaps(q, b, .ivx_bounds_flags(b))
  .ivx_run_relation_query(x, spec, mode = "peek", which = peek_which)
}

# Runtime: O(log n + c + k), where c = candidate count and k = matched count (worst-case O(n)).
# Containing relation query (peek) endpoint.
# **Inputs:** `x`; scalar `start`/`end`; `which`; optional `bounds`.
# **Outputs:** payload item (first) or interval_index slice (all).
# **Used by:** users/tests.
#' Peek intervals containing a query interval
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param which One of `"first"` or `"all"`.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return For `which = "first"`, the payload item from the first match (or
#'   `NULL` on no match). For `which = "all"`, an `interval_index` slice
#'   of matches.
#' @export
peek_containing <- function(x, start, end, which = c("first", "all"), bounds = NULL) {
  .ivx_assert_index(x)
  peek_which <- match.arg(which)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))
  spec <- .ivx_spec_containing(q, b, .ivx_bounds_flags(b))
  .ivx_run_relation_query(x, spec, mode = "peek", which = peek_which)
}

# Runtime: O(log n + c + k), where c = candidate count and k = matched count (worst-case O(n)).
# Within relation query (peek) endpoint.
# **Inputs:** `x`; scalar `start`/`end`; `which`; optional `bounds`.
# **Outputs:** payload item (first) or interval_index slice (all).
# **Used by:** users/tests.
#' Peek intervals within a query interval
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param which One of `"first"` or `"all"`.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return For `which = "first"`, the payload item from the first match (or
#'   `NULL` on no match). For `which = "all"`, an `interval_index` slice
#'   of matches.
#' @export
peek_within <- function(x, start, end, which = c("first", "all"), bounds = NULL) {
  .ivx_assert_index(x)
  peek_which <- match.arg(which)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))
  spec <- .ivx_spec_within(q, b, .ivx_bounds_flags(b))
  .ivx_run_relation_query(x, spec, mode = "peek", which = peek_which)
}

# Runtime: O(log n + c) for `which = "first"`; O(n log n) for `which = "all"`.
# Overlap relation query (pop) endpoint.
# **Inputs:** `x`; scalar `start`/`end`; `which`; optional `bounds`.
# **Outputs:** pop result list with `element/start/end/remaining`.
# **Used by:** users/tests.
#' Pop overlapping intervals
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param which One of `"first"` or `"all"`.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return For `which = "first"`, a list with `element`, `start`, `end`, `remaining`.
#'   For `which = "all"`, `element` is an `interval_index` slice and
#'   `start`/`end` are `NULL`.
#' @export
pop_overlaps <- function(x, start, end, which = c("first", "all"), bounds = NULL) {
  .ivx_assert_index(x)
  pop_which <- match.arg(which)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))
  spec <- .ivx_spec_overlaps(q, b, .ivx_bounds_flags(b))
  .ivx_run_relation_query(x, spec, mode = "pop", which = pop_which)
}

# Runtime: O(log n + c) for `which = "first"`; O(n log n) for `which = "all"`.
# Containing relation query (pop) endpoint.
# **Inputs:** `x`; scalar `start`/`end`; `which`; optional `bounds`.
# **Outputs:** pop result list with `element/start/end/remaining`.
# **Used by:** users/tests.
#' Pop intervals containing a query interval
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param which One of `"first"` or `"all"`.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return For `which = "first"`, a list with `element`, `start`, `end`, `remaining`.
#'   For `which = "all"`, `element` is an `interval_index` slice and
#'   `start`/`end` are `NULL`.
#' @export
pop_containing <- function(x, start, end, which = c("first", "all"), bounds = NULL) {
  .ivx_assert_index(x)
  pop_which <- match.arg(which)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))
  spec <- .ivx_spec_containing(q, b, .ivx_bounds_flags(b))
  .ivx_run_relation_query(x, spec, mode = "pop", which = pop_which)
}

# Runtime: O(log n + c) for `which = "first"`; O(n log n) for `which = "all"`.
# Within relation query (pop) endpoint.
# **Inputs:** `x`; scalar `start`/`end`; `which`; optional `bounds`.
# **Outputs:** pop result list with `element/start/end/remaining`.
# **Used by:** users/tests.
#' Pop intervals within a query interval
#'
#' @param x An `interval_index`.
#' @param start Query interval start.
#' @param end Query interval end.
#' @param which One of `"first"` or `"all"`.
#' @param bounds Optional boundary override. One of `"[)"`, `"[]"`, `"()"`, `"(]"`.
#' @return For `which = "first"`, a list with `element`, `start`, `end`, `remaining`.
#'   For `which = "all"`, `element` is an `interval_index` slice and
#'   `start`/`end` are `NULL`.
#' @export
pop_within <- function(x, start, end, which = c("first", "all"), bounds = NULL) {
  .ivx_assert_index(x)
  pop_which <- match.arg(which)
  b <- .ivx_resolve_bounds(x, bounds)
  q <- .ivx_normalize_interval(start, end, endpoint_type = .ivx_endpoint_type_state(x))
  spec <- .ivx_spec_within(q, b, .ivx_bounds_flags(b))
  .ivx_run_relation_query(x, spec, mode = "pop", which = pop_which)
}
