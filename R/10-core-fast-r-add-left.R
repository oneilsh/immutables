# Runtime: O(log n) per insertion near left edge.
# Fast non-lambda implementation for hot prepend/concat paths.
.add_left_fast <- function(t, el, monoids) {
  if(t %isa% Empty) {
    return(.measured_single_fast(el, monoids))
  }
  if(t %isa% Single) {
    return(.measured_deep_fast(
      .measured_digit_fast(list(el), monoids = monoids),
      .measured_empty_fast(monoids),
      .measured_digit_fast(list(.subset2(t, 1)), monoids = monoids),
      monoids
    ))
  }
  if(t %isa% Digit) {
    return(.measured_digit_fast(c(list(el), as.list(t)), monoids = monoids))
  }
  if(t %isa% Deep) {
    if(length(.subset2(t, "prefix")) == 4) {
      new_prefix <- .measured_digit_fast(list(el, .subset2(t, "prefix")[[1]]), monoids = monoids)
      new_middle_node <- .measured_node3_fast(
        .subset2(t, "prefix")[[2]],
        .subset2(t, "prefix")[[3]],
        .subset2(t, "prefix")[[4]],
        monoids
      )
      new_middle <- .add_left_fast(.subset2(t, "middle"), new_middle_node, monoids)
      return(.measured_deep_fast(prefix = new_prefix, middle = new_middle, suffix = .subset2(t, "suffix"), monoids))
    }
    new_prefix <- .add_left_fast(.subset2(t, "prefix"), el, monoids)
    return(.measured_deep_fast(prefix = new_prefix, middle = .subset2(t, "middle"), suffix = .subset2(t, "suffix"), monoids))
  }
  stop("Unsupported node type in .add_left_fast().")
}

# Runtime: O(k log n), where k = length(els).
.add_all_left_fast <- function(t, els, monoids) {
  if(length(els) == 0L) {
    return(t)
  }
  for(i in length(els):1L) {
    t <- .add_left_fast(t, els[[i]], monoids)
  }
  t
}
