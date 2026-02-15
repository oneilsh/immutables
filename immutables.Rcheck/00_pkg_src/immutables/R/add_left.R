# Runtime: O(n) worst-case in relevant input/subtree size.
add_left(e, el, monoids) %::% Empty : . : list : Single
add_left(e, el, monoids) %as% {
  measured_single(el, monoids)
}

# Runtime: O(n) worst-case in relevant input/subtree size.
add_left(s, el, monoids) %::% Single : . : list : Deep
add_left(s, el, monoids) %as% {
  measured_deep(
    measured_digit(el, monoids = monoids),
    measured_empty(monoids),
    measured_digit(.subset2(s, 1), monoids = monoids),
    monoids
  )
}

# this is actually a pain, because we implement digits as lists with various attributes
# we can't just c() the element to the list, as we'd get a list(el, old_list), rather than list(el, old_list[[1]], old_list[[2]]), etc.
# We prepend via c(list(el), d) and then restore class attributes.
# Runtime: O(n) worst-case in relevant input/subtree size.
add_left(d, el, monoids) %::% Digit : . : list : Digit
add_left(d, el, monoids) %as% {
  oldclasses <- class(d)
  newd <- c(list(el), d)
  class(newd) <- oldclasses
  set_measure(newd, monoids)
}



# this is where the magic happens; 
# if the prefix digit has 4 elements: then we push off the last to 
# be stored deeper along inside a Node3, and just store the new el and the remaining 1 from the digit in the prefix as a digit.
# otherwise: it's a simple add to the prefix digit.
# Runtime: O(n) worst-case in relevant input/subtree size.
add_left(d, el, monoids) %::% Deep : . : list : Deep
add_left(d, el, monoids) %as% {
  if(length(.subset2(d,"prefix")) == 4) {
    new_prefix <- measured_digit(el, .subset2(d,"prefix")[[1]], monoids = monoids)
    new_middle_node <- measured_node3(.subset2(d,"prefix")[[2]], .subset2(d,"prefix")[[3]], .subset2(d,"prefix")[[4]], monoids)
    new_middle <- add_left(.subset2(d,"middle"), new_middle_node, monoids)
    measured_deep(prefix = new_prefix, middle = new_middle, suffix = .subset2(d,"suffix"), monoids)
  } else {
    new_prefix <- add_left(.subset2(d,"prefix"), el, monoids)
    measured_deep(prefix = new_prefix, middle = .subset2(d,"middle"), suffix = .subset2(d,"suffix"), monoids)
  }
}


# Runtime: O(n) worst-case in relevant input/subtree size.
add_all_left(t, els, monoids) %::% FingerTree : . : list : FingerTree
add_all_left(t, els, monoids) %as% {
  for(el in rev(els)) {
    t <- add_left(t, el, monoids)
  }
  return(t)
}

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
