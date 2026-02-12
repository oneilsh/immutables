##
## app3 is what the original paper called this generalized concatenation function used by concat()
##

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

# Runtime: O(log n + length(ts)) in typical balanced usage, with recursion
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

## concatenation, depending on what kinds of finger trees we want to concatenate we do different things
# each concat function also takes a list of elements to smush between the two trees, useful for later functionality
# generalized concatenation helper; inserts list ts between two trees
# Runtime: see `.app3_fast`.
app3(e, ts, xs, monoids) %::% Empty : list : FingerTree : list : FingerTree
app3(e, ts, xs, monoids) %as% .app3_fast(e, ts, xs, monoids)

# Runtime: see `.app3_fast`.
app3(xs, ts, e, monoids) %::% FingerTree : list : Empty : list : FingerTree
app3(xs, ts, e, monoids) %as% .app3_fast(xs, ts, e, monoids)

# Runtime: see `.app3_fast`.
app3(x, ts, xs, monoids) %::% Single : list : FingerTree : list : FingerTree
app3(x, ts, xs, monoids) %as% .app3_fast(x, ts, xs, monoids)

# Runtime: see `.app3_fast`.
app3(xs, ts, x, monoids) %::% FingerTree : list : Single : list : FingerTree
app3(xs, ts, x, monoids) %as% .app3_fast(xs, ts, x, monoids)

# Runtime: see `.app3_fast`.
app3(xs, ts, ys, monoids) %::% Deep : list : Deep : list : FingerTree
app3(xs, ts, ys, monoids) %as% .app3_fast(xs, ts, ys, monoids)

# Runtime: O(n) worst-case in relevant input/subtree size.
concat(xs, ys, monoids) %::% FingerTree : FingerTree : list : FingerTree
concat(xs, ys, monoids) %as% {
  .app3_fast(xs, list(), ys, monoids)
}
