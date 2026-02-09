# helpers for converting between digits/nodes/trees and for deconstructing trees

# construct a Digit from a list of elements.
# input: xs list of 1..4 elements (raw elements or nodes), optional measure reducer r.
# output: Digit(xs...) with preserved order, measured if r is a MeasureReducer.
# note: empty input returns an empty list() sentinel, used by deepL/deepR rebuild logic.
build_digit <- function(xs, r = NULL) {
  if(length(xs) == 0) {
    return(list())
  }
  if(!is.null(r) && is_measure_reducer(r)) {
    return(do.call(measured_digit, c(xs, list(r = r))))
  }
  do.call(Digit, xs)
}

# build a Deep node from prefix digit, middle tree, and suffix digit.
# input: pr (Digit), m (FingerTree), sf (Digit), optional measure reducer r.
# output: Deep(pr, m, sf), measured if r is a MeasureReducer.
build_deep <- function(pr, m, sf, r = NULL) {
  if(!is.null(r) && is_measure_reducer(r)) {
    return(measured_deep(pr, m, sf, r))
  }
  Deep(pr, m, sf)
}

# convert a small list/digit (size 0..4) into a valid FingerTree shape.
# input: d list of elements (typically a Digit), optional measure reducer r.
# output: Empty / Single / Deep representing the same left-to-right sequence.
digit_to_tree <- function(d, r = NULL) {
  n <- length(d)
  if(n == 0) {
    return(if(!is.null(r) && is_measure_reducer(r)) measured_empty(r) else Empty())
  }
  if(n == 1) {
    return(if(!is.null(r) && is_measure_reducer(r)) measured_single(d[[1]], r) else Single(d[[1]]))
  }
  if(n == 2) {
    pr <- build_digit(list(d[[1]]), r)
    sf <- build_digit(list(d[[2]]), r)
    return(build_deep(pr, if(!is.null(r) && is_measure_reducer(r)) measured_empty(r) else Empty(), sf, r))
  }
  if(n == 3) {
    pr <- build_digit(list(d[[1]], d[[2]]), r)
    sf <- build_digit(list(d[[3]]), r)
    return(build_deep(pr, if(!is.null(r) && is_measure_reducer(r)) measured_empty(r) else Empty(), sf, r))
  }
  if(n == 4) {
    pr <- build_digit(list(d[[1]], d[[2]]), r)
    sf <- build_digit(list(d[[3]], d[[4]]), r)
    return(build_deep(pr, if(!is.null(r) && is_measure_reducer(r)) measured_empty(r) else Empty(), sf, r))
  }
  stop("digit_to_tree expects a digit of size 0..4")
}

# convert a Node2/Node3 into a Digit of its children.
# input: node (Node2 or Node3), optional measure reducer r.
# output: Digit with 2 or 3 elements in the same order.
node_to_digit <- function(node, r = NULL) {
  if(!is_structural_node(node)) {
    stop("node_to_digit expects a Node2/Node3")
  }
  build_digit(as.list(node), r)
}

# viewL: return leftmost element and the remaining tree
# input: t non-empty FingerTree, optional measure reducer r.
# output: list(elem = leftmost element of t, rest = t without that element).
viewL <- function(t, r = NULL) {
  if(t %isa% Empty) {
    stop("viewL on Empty")
  }
  if(t %isa% Single) {
    return(list(elem = t[[1]], rest = if(!is.null(r) && is_measure_reducer(r)) measured_empty(r) else Empty()))
  }
  # Deep
  pr <- t$prefix
  if(length(pr) > 1) {
    head <- pr[[1]]
    tail <- pr[2:length(pr)]
    new_pr <- build_digit(tail, r)
    return(list(elem = head, rest = build_deep(new_pr, t$middle, t$suffix, r)))
  }
  # prefix has exactly one element
  head <- pr[[1]]
  m <- t$middle
  if(m %isa% Empty) {
    return(list(elem = head, rest = digit_to_tree(t$suffix, r)))
  }
  # pull from middle
  res <- viewL(m, r)
  node <- res$elem
  m_rest <- res$rest
  new_pr <- node_to_digit(node, r)
  return(list(elem = head, rest = build_deep(new_pr, m_rest, t$suffix, r)))
}

# viewR: return rightmost element and the remaining tree
# input: t non-empty FingerTree, optional measure reducer r.
# output: list(elem = rightmost element of t, rest = t without that element).
viewR <- function(t, r = NULL) {
  if(t %isa% Empty) {
    stop("viewR on Empty")
  }
  if(t %isa% Single) {
    return(list(elem = t[[1]], rest = if(!is.null(r) && is_measure_reducer(r)) measured_empty(r) else Empty()))
  }
  # Deep
  sf <- t$suffix
  if(length(sf) > 1) {
    head <- sf[[length(sf)]]
    tail <- sf[1:(length(sf) - 1)]
    new_sf <- build_digit(tail, r)
    return(list(elem = head, rest = build_deep(t$prefix, t$middle, new_sf, r)))
  }
  # suffix has exactly one element
  head <- sf[[1]]
  m <- t$middle
  if(m %isa% Empty) {
    return(list(elem = head, rest = digit_to_tree(t$prefix, r)))
  }
  # pull from middle (rightmost)
  res <- viewR(m, r)
  node <- res$elem
  m_rest <- res$rest
  new_sf <- node_to_digit(node, r)
  return(list(elem = head, rest = build_deep(t$prefix, m_rest, new_sf, r)))
}

# deepL: rebuild Deep, possibly pulling from middle if prefix is empty
# input: pr digit/list for prefix, m middle FingerTree of nodes, sf suffix digit/list.
# output: a valid FingerTree preserving order; if pr is empty it borrows from m or
# collapses to a tree built from sf.
deepL <- function(pr, m, sf, r = NULL) {
  if(length(pr) > 0) {
    return(build_deep(pr, m, sf, r))
  }
  if(m %isa% Empty) {
    return(digit_to_tree(sf, r))
  }
  res <- viewL(m, r)
  node <- res$elem
  m_rest <- res$rest
  new_pr <- node_to_digit(node, r)
  build_deep(new_pr, m_rest, sf, r)
}

# deepR: rebuild Deep, possibly pulling from middle if suffix is empty
# input: pr prefix digit/list, m middle FingerTree of nodes, sf suffix digit/list.
# output: a valid FingerTree preserving order; if sf is empty it borrows from m or
# collapses to a tree built from pr.
deepR <- function(pr, m, sf, r = NULL) {
  if(length(sf) > 0) {
    return(build_deep(pr, m, sf, r))
  }
  if(m %isa% Empty) {
    return(digit_to_tree(pr, r))
  }
  res <- viewR(m, r)
  node <- res$elem
  m_rest <- res$rest
  new_sf <- node_to_digit(node, r)
  build_deep(pr, m_rest, new_sf, r)
}
