# Runtime: O(k), where k = length(xs).
.measured_nodes_fast <- function(xs, monoids) {
  n <- length(xs)
  if(n < 2L) {
    stop("measured_nodes requires at least two elements.")
  }

  out <- list()
  out_i <- 1L
  i <- 1L

  repeat {
    rem <- n - i + 1L
    if(rem == 2L) {
      out[[out_i]] <- .measured_node2_fast(xs[[i]], xs[[i + 1L]], monoids)
      break
    }
    if(rem == 3L) {
      out[[out_i]] <- .measured_node3_fast(xs[[i]], xs[[i + 1L]], xs[[i + 2L]], monoids)
      break
    }
    if(rem == 4L) {
      out[[out_i]] <- .measured_node2_fast(xs[[i]], xs[[i + 1L]], monoids)
      out[[out_i + 1L]] <- .measured_node2_fast(xs[[i + 2L]], xs[[i + 3L]], monoids)
      break
    }

    out[[out_i]] <- .measured_node3_fast(xs[[i]], xs[[i + 1L]], xs[[i + 2L]], monoids)
    i <- i + 3L
    out_i <- out_i + 1L
  }

  out
}

# Runtime: O(log n + k) in typical balanced usage, where k = length(ts), with recursion
# depth proportional to concatenation spine depth.
.app3_fast <- function(xs, ts, ys, monoids) {
  if(xs %isa% Empty) {
    return(.add_all_left_fast(ys, ts, monoids))
  }
  if(ys %isa% Empty) {
    return(.add_all_right_fast(xs, ts, monoids))
  }
  if(xs %isa% Single) {
    return(.add_left_fast(.add_all_left_fast(ys, ts, monoids), .subset2(xs, 1), monoids))
  }
  if(ys %isa% Single) {
    return(.add_right_fast(.add_all_right_fast(xs, ts, monoids), .subset2(ys, 1), monoids))
  }

  mid_ts <- .measured_nodes_fast(
    c(as.list(.subset2(xs, "suffix")), ts, as.list(.subset2(ys, "prefix"))),
    monoids
  )
  .measured_deep_fast(
    .subset2(xs, "prefix"),
    .app3_fast(.subset2(xs, "middle"), mid_ts, .subset2(ys, "middle"), monoids),
    .subset2(ys, "suffix"),
    monoids
  )
}
