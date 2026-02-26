#SO

# Query-spec builders return a plan consumed by .ivx_run_relation_query():
# - lower: optional lower start-bound value for candidate windowing (NULL = no bound).
# - lower_strict: FALSE => start >= lower, TRUE => start > lower.
# - upper: optional upper start-bound value for candidate windowing (NULL = no bound).
# - upper_strict: FALSE => start <= upper, TRUE => start < upper.
# - no_match_subtree(node): prune predicate over subtree measures; TRUE means
#   the subtree cannot contain matches and traversal can skip it.
# - leaf_match(entry): exact entry-level relation test run on remaining candidates.

# Point relation spec (entry interval contains query point).
# **Inputs:** `qp` normalized point query list; scalar `bounds`; bounds-flag list `flags`.
# **Outputs:** query-spec list(lower/lower_strict/upper/upper_strict/no_match_subtree/leaf_match).
# **Used by:** peek_point(), pop_point().
.ivx_spec_point <- function(qp, bounds, flags) {
  include_start <- isTRUE(flags$include_start)
  include_end <- isTRUE(flags$include_end)

  leaf_match <- if(.ivx_is_fast_endpoint_type(qp$endpoint_type)) {
    function(e) {
      left_ok <- if(include_start) isTRUE(qp$value >= e$start) else isTRUE(qp$value > e$start)
      right_ok <- if(include_end) isTRUE(qp$value <= e$end) else isTRUE(qp$value < e$end)
      isTRUE(left_ok && right_ok)
    }
  } else {
    function(e) .ivx_contains_point(e$start, e$end, qp$value, bounds, qp$endpoint_type)
  }

  list(
    lower = NULL,
    lower_strict = FALSE,
    upper = qp$value,
    upper_strict = FALSE,
    no_match_subtree = function(node) {
      m <- node_measure(node, ".ivx_max_end")
      if(!isTRUE(m$has)) {
        return(TRUE)
      }
      cmp <- .ivx_compare_scalar_fast(m$end, qp$value, endpoint_type = qp$endpoint_type)
      if(include_end) {
        cmp < 0L
      } else {
        cmp <= 0L
      }
    },
    leaf_match = leaf_match
  )
}

# Runtime: O(1).
# Overlap relation spec (entry overlaps query interval under current bounds).
# **Inputs:** `q` normalized interval query list; scalar `bounds`; bounds-flag list `flags`.
# **Outputs:** query-spec list(lower/lower_strict/upper/upper_strict/no_match_subtree/leaf_match).
# **Used by:** peek_overlaps(), pop_overlaps().
.ivx_spec_overlaps <- function(q, bounds, flags) {
  touching_is_overlap <- isTRUE(flags$include_start) && isTRUE(flags$include_end)

  leaf_match <- if(.ivx_is_fast_endpoint_type(q$endpoint_type)) {
    function(e) {
      a_before_b <- if(touching_is_overlap) isTRUE(e$end < q$start) else isTRUE(e$end <= q$start)
      b_before_a <- if(touching_is_overlap) isTRUE(q$end < e$start) else isTRUE(q$end <= e$start)
      !isTRUE(a_before_b || b_before_a)
    }
  } else {
    function(e) .ivx_overlaps_interval(e$start, e$end, q$start, q$end, bounds, q$endpoint_type)
  }

  list(
    lower = NULL,
    lower_strict = FALSE,
    upper = q$end,
    upper_strict = !isTRUE(touching_is_overlap),
    no_match_subtree = function(node) {
      m <- node_measure(node, ".ivx_max_end")
      if(!isTRUE(m$has)) {
        return(TRUE)
      }
      cmp <- .ivx_compare_scalar_fast(m$end, q$start, endpoint_type = q$endpoint_type)
      if(touching_is_overlap) {
        cmp < 0L
      } else {
        cmp <= 0L
      }
    },
    leaf_match = leaf_match
  )
}

# Runtime: O(1).
# Containing relation spec (entry contains query interval).
# **Inputs:** `q` normalized interval query list; scalar `bounds`; bounds-flag list `flags`.
# **Outputs:** query-spec list(lower/lower_strict/upper/upper_strict/no_match_subtree/leaf_match).
# **Used by:** peek_containing(), pop_containing().
.ivx_spec_containing <- function(q, bounds, flags) {
  touching_is_overlap <- isTRUE(flags$include_start) && isTRUE(flags$include_end)

  leaf_match <- if(.ivx_is_fast_endpoint_type(q$endpoint_type)) {
    function(e) {
      contains <- isTRUE(e$start <= q$start) && isTRUE(e$end >= q$end)
      if(!contains) {
        return(FALSE)
      }
      a_before_b <- if(touching_is_overlap) isTRUE(e$end < q$start) else isTRUE(e$end <= q$start)
      b_before_a <- if(touching_is_overlap) isTRUE(q$end < e$start) else isTRUE(q$end <= e$start)
      !isTRUE(a_before_b || b_before_a)
    }
  } else {
    function(e) {
      .ivx_overlaps_interval(e$start, e$end, q$start, q$end, bounds, q$endpoint_type) &&
        .ivx_contains_interval(e$start, e$end, q$start, q$end, q$endpoint_type)
    }
  }

  list(
    lower = NULL,
    lower_strict = FALSE,
    upper = q$start,
    upper_strict = FALSE,
    no_match_subtree = function(node) {
      m <- node_measure(node, ".ivx_max_end")
      if(!isTRUE(m$has)) {
        return(TRUE)
      }
      .ivx_compare_scalar_fast(m$end, q$end, endpoint_type = q$endpoint_type) < 0L
    },
    leaf_match = leaf_match
  )
}

# Runtime: O(1).
# Within relation spec (entry is within query interval).
# **Inputs:** `q` normalized interval query list; scalar `bounds`; bounds-flag list `flags`.
# **Outputs:** query-spec list(lower/lower_strict/upper/upper_strict/no_match_subtree/leaf_match).
# **Used by:** peek_within(), pop_within().
.ivx_spec_within <- function(q, bounds, flags) {
  touching_is_overlap <- isTRUE(flags$include_start) && isTRUE(flags$include_end)

  leaf_match <- if(.ivx_is_fast_endpoint_type(q$endpoint_type)) {
    function(e) {
      within <- isTRUE(q$start <= e$start) && isTRUE(q$end >= e$end)
      if(!within) {
        return(FALSE)
      }
      a_before_b <- if(touching_is_overlap) isTRUE(e$end < q$start) else isTRUE(e$end <= q$start)
      b_before_a <- if(touching_is_overlap) isTRUE(q$end < e$start) else isTRUE(q$end <= e$start)
      !isTRUE(a_before_b || b_before_a)
    }
  } else {
    function(e) {
      .ivx_overlaps_interval(e$start, e$end, q$start, q$end, bounds, q$endpoint_type) &&
        .ivx_contains_interval(q$start, q$end, e$start, e$end, q$endpoint_type)
    }
  }

  list(
    lower = q$start,
    lower_strict = FALSE,
    upper = q$end,
    upper_strict = !isTRUE(touching_is_overlap),
    no_match_subtree = function(node) {
      mmin <- node_measure(node, ".ivx_min_end")
      mmax <- node_measure(node, ".ivx_max_end")
      if(!isTRUE(mmin$has) || !isTRUE(mmax$has)) {
        return(TRUE)
      }

      # No interval in subtree can satisfy end <= q.end.
      if(.ivx_compare_scalar_fast(mmin$end, q$end, endpoint_type = q$endpoint_type) > 0L) {
        return(TRUE)
      }

      # Even the largest end is too far left to overlap q.
      cmp <- .ivx_compare_scalar_fast(mmax$end, q$start, endpoint_type = q$endpoint_type)
      if(touching_is_overlap) {
        cmp < 0L
      } else {
        cmp <= 0L
      }
    },
    leaf_match = leaf_match
  )
}
