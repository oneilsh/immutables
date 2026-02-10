# helpers for converting between digits/nodes/trees and for deconstructing trees

# construct a Digit from a list of elements.
# input: xs list of 1..4 elements (raw elements or nodes), measure monoid r.
# output: measured Digit(xs...) with preserved order.
# note: empty input returns an empty list() sentinel, used by deepL/deepR rebuild logic.
build_digit(xs, r) %::% list : MeasureMonoid : .
build_digit(xs, r) %as% {
  if(length(xs) == 0) {
    return(list())
  }
  do.call(measured_digit, c(xs, list(r = r)))
}

# build a Deep node from prefix digit, middle tree, and suffix digit.
# input: pr (Digit), m (FingerTree), sf (Digit), measure monoid r.
# output: measured Deep(pr, m, sf).
build_deep(pr, m, sf, r) %::% Digit : FingerTree : Digit : MeasureMonoid : Deep
build_deep(pr, m, sf, r) %as% measured_deep(pr, m, sf, r)

# convert a small list/digit (size 0..4) into a valid measured FingerTree shape.
digit_to_tree(d, r) %::% list : MeasureMonoid : FingerTree
digit_to_tree(d, r) %as% {
  n <- length(d)
  if(n == 0) { return(measured_empty(r)) }
  if(n == 1) { return(measured_single(d[[1]], r)) }
  if(n == 2) {
    pr <- build_digit(list(d[[1]]), r)
    sf <- build_digit(list(d[[2]]), r)
    return(build_deep(pr, measured_empty(r), sf, r))
  }
  if(n == 3) {
    pr <- build_digit(list(d[[1]], d[[2]]), r)
    sf <- build_digit(list(d[[3]]), r)
    return(build_deep(pr, measured_empty(r), sf, r))
  }
  if(n == 4) {
    pr <- build_digit(list(d[[1]], d[[2]]), r)
    sf <- build_digit(list(d[[3]], d[[4]]), r)
    return(build_deep(pr, measured_empty(r), sf, r))
  }
  stop("digit_to_tree expects a digit of size 0..4")
}

# convert a Node2/Node3 into a measured Digit of its children.
node_to_digit(node, r) %::% Node : MeasureMonoid : Digit
node_to_digit(node, r) %as% build_digit(as.list(node), r)

# viewL: return leftmost element and the remaining tree
# input: t non-empty FingerTree, measure monoid r.
# output: list(elem = leftmost element of t, rest = t without that element).
viewL(t, r) %::% FingerTree : MeasureMonoid : list
viewL(t, r) %as% {
  if(t %isa% Empty) {
    stop("viewL on Empty")
  }
  if(t %isa% Single) {
    return(list(elem = .subset2(t, 1), rest = measured_empty(r)))
  }
  pr <- t$prefix
  if(length(pr) > 1) {
    head <- pr[[1]]
    tail <- pr[2:length(pr)]
    new_pr <- build_digit(tail, r)
    return(list(elem = head, rest = build_deep(new_pr, t$middle, t$suffix, r)))
  }
  head <- pr[[1]]
  m <- t$middle
  if(m %isa% Empty) {
    return(list(elem = head, rest = digit_to_tree(t$suffix, r)))
  }
  res <- viewL(m, r)
  node <- res$elem
  m_rest <- res$rest
  new_pr <- node_to_digit(node, r)
  list(elem = head, rest = build_deep(new_pr, m_rest, t$suffix, r))
}

# viewR: return rightmost element and the remaining tree
# input: t non-empty FingerTree, measure monoid r.
# output: list(elem = rightmost element of t, rest = t without that element).
viewR(t, r) %::% FingerTree : MeasureMonoid : list
viewR(t, r) %as% {
  if(t %isa% Empty) {
    stop("viewR on Empty")
  }
  if(t %isa% Single) {
    return(list(elem = .subset2(t, 1), rest = measured_empty(r)))
  }
  sf <- t$suffix
  if(length(sf) > 1) {
    head <- sf[[length(sf)]]
    tail <- sf[1:(length(sf) - 1)]
    new_sf <- build_digit(tail, r)
    return(list(elem = head, rest = build_deep(t$prefix, t$middle, new_sf, r)))
  }
  head <- sf[[1]]
  m <- t$middle
  if(m %isa% Empty) {
    return(list(elem = head, rest = digit_to_tree(t$prefix, r)))
  }
  res <- viewR(m, r)
  node <- res$elem
  m_rest <- res$rest
  new_sf <- node_to_digit(node, r)
  list(elem = head, rest = build_deep(t$prefix, m_rest, new_sf, r))
}

# deepL: rebuild Deep, possibly pulling from middle if prefix is empty
# input: pr digit/list for prefix, m middle FingerTree of nodes, sf suffix digit/list.
# output: a valid FingerTree preserving order; if pr is empty it borrows from m or
# collapses to a tree built from sf.
deepL(pr, m, sf, r) %::% . : FingerTree : . : MeasureMonoid : FingerTree
deepL(pr, m, sf, r) %as% {
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
deepR(pr, m, sf, r) %::% . : FingerTree : . : MeasureMonoid : FingerTree
deepR(pr, m, sf, r) %as% {
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
