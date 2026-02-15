# Runtime: O(1) for raw elements, O(1) for structural children with cache hit.
.measure_child_fast <- function(child, nm, r) {
  if(is_structural_node(child)) {
    attr(child, "measures", exact = TRUE)[[nm]]
  } else {
    r$measure(child)
  }
}

# Runtime: O(m * c), where m is monoid count and c is child count (small constant here).
.compute_measures_for_children_fast <- function(children, monoids) {
  out <- vector("list", length(monoids))
  names(out) <- names(monoids)
  for(nm in names(monoids)) {
    r <- monoids[[nm]]
    acc <- r$i
    for(ch in children) {
      acc <- r$f(acc, .measure_child_fast(ch, nm, r))
    }
    out[[nm]] <- acc
  }
  out
}

# Runtime: O(m), where m is monoid count.
.measured_empty_fast <- function(monoids) {
  e <- Empty()
  attr(e, "monoids") <- monoids
  out <- lapply(monoids, function(r) r$i)
  attr(e, "measures") <- out
  e
}

# Runtime: O(m), where m is monoid count.
.measured_single_fast <- function(el, monoids) {
  s <- Single(el)
  attr(s, "monoids") <- monoids
  out <- vector("list", length(monoids))
  names(out) <- names(monoids)
  for(nm in names(monoids)) {
    r <- monoids[[nm]]
    out[[nm]] <- .measure_child_fast(el, nm, r)
  }
  attr(s, "measures") <- out
  s
}

# Runtime: O(m * k), where k in [1,4].
.measured_digit_fast <- function(children, monoids) {
  d <- do.call(Digit, children)
  attr(d, "monoids") <- monoids
  attr(d, "measures") <- .compute_measures_for_children_fast(children, monoids)
  d
}

# Runtime: O(m).
.measured_node2_fast <- function(a, b, monoids) {
  n <- Node2(a, b)
  attr(n, "monoids") <- monoids
  attr(n, "measures") <- .compute_measures_for_children_fast(list(a, b), monoids)
  n
}

# Runtime: O(m).
.measured_node3_fast <- function(a, b, c, monoids) {
  n <- Node3(a, b, c)
  attr(n, "monoids") <- monoids
  attr(n, "measures") <- .compute_measures_for_children_fast(list(a, b, c), monoids)
  n
}

# Runtime: O(m), where m is number of monoids.
# Fast constructor for Deep nodes that reuses cached child measures directly.
.measured_deep_fast <- function(prefix, middle, suffix, monoids) {
  t <- Deep(prefix, middle, suffix)
  attr(t, "monoids") <- monoids
  out <- vector("list", length(monoids))
  names(out) <- names(monoids)
  pm <- attr(prefix, "measures", exact = TRUE)
  mm <- attr(middle, "measures", exact = TRUE)
  sm <- attr(suffix, "measures", exact = TRUE)
  for(nm in names(monoids)) {
    r <- monoids[[nm]]
    out[[nm]] <- r$f(r$f(pm[[nm]], mm[[nm]]), sm[[nm]])
  }
  attr(t, "measures") <- out
  t
}

# Runtime: O(log n) per insertion near right edge.
# Fast non-lambda implementation for hot append/build paths.
.add_right_fast <- function(t, el, monoids) {
  if(t %isa% Empty) {
    return(.measured_single_fast(el, monoids))
  }
  if(t %isa% Single) {
    return(.measured_deep_fast(
      .measured_digit_fast(list(.subset2(t, 1)), monoids = monoids),
      .measured_empty_fast(monoids),
      .measured_digit_fast(list(el), monoids = monoids),
      monoids
    ))
  }
  if(t %isa% Digit) {
    return(.measured_digit_fast(c(as.list(t), list(el)), monoids = monoids))
  }
  if(t %isa% Deep) {
    if(length(.subset2(t, "suffix")) == 4) {
      new_suffix <- .measured_digit_fast(list(.subset2(t, "suffix")[[4]], el), monoids = monoids)
      new_middle_node <- .measured_node3_fast(
        .subset2(t, "suffix")[[1]],
        .subset2(t, "suffix")[[2]],
        .subset2(t, "suffix")[[3]],
        monoids
      )
      new_middle <- .add_right_fast(.subset2(t, "middle"), new_middle_node, monoids)
      return(.measured_deep_fast(prefix = .subset2(t, "prefix"), middle = new_middle, suffix = new_suffix, monoids))
    }
    new_suffix <- .add_right_fast(.subset2(t, "suffix"), el, monoids)
    return(.measured_deep_fast(prefix = .subset2(t, "prefix"), middle = .subset2(t, "middle"), suffix = new_suffix, monoids))
  }
  stop("Unsupported node type in .add_right_fast().")
}

# Runtime: O(k log n), where k = length(els).
.add_all_right_fast <- function(t, els, monoids) {
  if(length(els) == 0L) {
    return(t)
  }
  for(el in els) {
    t <- .add_right_fast(t, el, monoids)
  }
  t
}
