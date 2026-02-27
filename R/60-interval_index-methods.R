#SO

# Runtime: O(n) from list materialization + linear rebuild.
# Internal cast-down builder for interval_index -> flexseq.
# **Inputs:** `x` interval_index; optional `monoids`.
# **Outputs:** flexseq of interval entry records.
# **Used by:** as_flexseq.interval_index().
.as_flexseq_build.interval_index <- function(x, monoids = NULL) {
  entries <- as.list.flexseq(x)
  out_monoids <- monoids
  if(is.null(out_monoids)) {
    ms <- attr(x, "monoids", exact = TRUE)
    out_monoids <- ms[setdiff(names(ms), c(".ivx_max_start", ".ivx_max_end", ".ivx_min_end", ".oms_max_key"))]
  }
  .as_flexseq_build.default(entries, monoids = out_monoids)
}

#' @method as_flexseq interval_index
#' @export
# Runtime: O(n) from list materialization + linear rebuild.
# Public cast to plain flexseq while dropping interval-specific behavior.
# **Inputs:** `x` interval_index.
# **Outputs:** flexseq.
# **Used by:** users/tests.
as_flexseq.interval_index <- function(x) {
  .as_flexseq_build.interval_index(x, monoids = NULL)
}

# Runtime: O(n).
# Coerce to payload list in interval order.
# **Inputs:** `x` interval_index.
# **Outputs:** base list of payload items with names preserved.
# **Used by:** users/tests.
#' Coerce Interval Index to List
#'
#' @method as.list interval_index
#' @param x An `interval_index`.
#' @param ... Unused.
#' @return A plain list of payload elements in interval order.
#' @export
as.list.interval_index <- function(x, ...) {
  .ivx_assert_index(x)
  entries <- as.list.flexseq(x, ...)
  out <- lapply(entries, function(e) e$item)
  names(out) <- names(entries)
  out
}

# Runtime: O(1).
# Length method backed by cached `.size` measure.
# **Inputs:** `x` interval_index.
# **Outputs:** scalar integer length.
# **Used by:** users/tests/base generics.
#' Interval Index Length
#'
#' @method length interval_index
#' @param x An `interval_index`.
#' @return Integer length.
#' @export
length.interval_index <- function(x) {
  as.integer(node_measure(x, ".size"))
}

# Plot method delegates to shared flexseq tree plotting.
# **Inputs:** `x` interval_index; `...`.
# **Outputs:** plot side-effect (invisible device result).
# **Used by:** users.
#' Plot an Interval Index Tree
#'
#' @method plot interval_index
#' @param x An `interval_index`.
#' @param ... Passed to the internal tree plotting routine.
#' @export
# Runtime: O(n) to build plot graph data.
plot.interval_index <- function(x, ...) {
  plot.flexseq(x, ...)
}
