# helpers for converting between digits/nodes/trees and for deconstructing trees

# attach canonical monoid set to a constructed subtree
with_tree_monoids(t, monoids) %::% FingerTree : list : FingerTree
with_tree_monoids(t, monoids) %as% {
  attr(t, "monoids") <- ensure_size_monoids(monoids)
  t
}

# construct a Digit from a list of elements.
# input: xs list of 1..4 elements (raw elements or nodes), measure monoid r.
# output: measured Digit(xs...) with preserved order.
# note: empty input returns an empty list() sentinel, used by deepL/deepR rebuild logic.
build_digit(xs, monoids) %::% list : list : .
build_digit(xs, monoids) %as% {
  if(length(xs) == 0) {
    return(list())
  }
  do.call(measured_digit, c(xs, list(monoids = monoids)))
}

# build a Deep node from prefix digit, middle tree, and suffix digit.
# input: pr (Digit), m (FingerTree), sf (Digit), measure monoid r.
# output: measured Deep(pr, m, sf).
build_deep(pr, m, sf, monoids) %::% Digit : FingerTree : Digit : list : Deep
build_deep(pr, m, sf, monoids) %as% with_tree_monoids(measured_deep(pr, m, sf, monoids), monoids)

# convert a small list/digit (size 0..4) into a valid measured FingerTree shape.
digit_to_tree(d, monoids) %::% list : list : FingerTree
digit_to_tree(d, monoids) %as% {
  n <- length(d)
  if(n == 0) { return(with_tree_monoids(measured_empty(monoids), monoids)) }
  if(n == 1) { return(with_tree_monoids(measured_single(d[[1]], monoids), monoids)) }
  if(n == 2) {
    pr <- build_digit(list(d[[1]]), monoids)
    sf <- build_digit(list(d[[2]]), monoids)
    return(build_deep(pr, measured_empty(monoids), sf, monoids))
  }
  if(n == 3) {
    pr <- build_digit(list(d[[1]], d[[2]]), monoids)
    sf <- build_digit(list(d[[3]]), monoids)
    return(build_deep(pr, measured_empty(monoids), sf, monoids))
  }
  if(n == 4) {
    pr <- build_digit(list(d[[1]], d[[2]]), monoids)
    sf <- build_digit(list(d[[3]], d[[4]]), monoids)
    return(build_deep(pr, measured_empty(monoids), sf, monoids))
  }
  stop("digit_to_tree expects a digit of size 0..4")
}

# convert a Node2/Node3 into a measured Digit of its children.
node_to_digit(node, monoids) %::% Node : list : Digit
node_to_digit(node, monoids) %as% build_digit(as.list(node), monoids)

# viewL: return leftmost element and the remaining tree
# input: t non-empty FingerTree, measure monoid r.
# output: list(elem = leftmost element of t, rest = t without that element).
viewL(t, monoids) %::% FingerTree : list : list
viewL(t, monoids) %as% {
  if(t %isa% Empty) {
    stop("viewL on Empty")
  }
  if(t %isa% Single) {
    return(list(elem = .subset2(t, 1), rest = with_tree_monoids(measured_empty(monoids), monoids)))
  }
  pr <- t$prefix
  if(length(pr) > 1) {
    head <- pr[[1]]
    tail <- pr[2:length(pr)]
    new_pr <- build_digit(tail, monoids)
    return(list(elem = head, rest = build_deep(new_pr, t$middle, t$suffix, monoids)))
  }
  head <- pr[[1]]
  m <- t$middle
  if(m %isa% Empty) {
    return(list(elem = head, rest = digit_to_tree(t$suffix, monoids)))
  }
  res <- viewL(m, monoids)
  node <- res$elem
  m_rest <- res$rest
  new_pr <- node_to_digit(node, monoids)
  list(elem = head, rest = build_deep(new_pr, m_rest, t$suffix, monoids))
}

# viewR: return rightmost element and the remaining tree
# input: t non-empty FingerTree, measure monoid r.
# output: list(elem = rightmost element of t, rest = t without that element).
viewR(t, monoids) %::% FingerTree : list : list
viewR(t, monoids) %as% {
  if(t %isa% Empty) {
    stop("viewR on Empty")
  }
  if(t %isa% Single) {
    return(list(elem = .subset2(t, 1), rest = with_tree_monoids(measured_empty(monoids), monoids)))
  }
  sf <- t$suffix
  if(length(sf) > 1) {
    head <- sf[[length(sf)]]
    tail <- sf[1:(length(sf) - 1)]
    new_sf <- build_digit(tail, monoids)
    return(list(elem = head, rest = build_deep(t$prefix, t$middle, new_sf, monoids)))
  }
  head <- sf[[1]]
  m <- t$middle
  if(m %isa% Empty) {
    return(list(elem = head, rest = digit_to_tree(t$prefix, monoids)))
  }
  res <- viewR(m, monoids)
  node <- res$elem
  m_rest <- res$rest
  new_sf <- node_to_digit(node, monoids)
  list(elem = head, rest = build_deep(t$prefix, m_rest, new_sf, monoids))
}

# deepL: rebuild Deep, possibly pulling from middle if prefix is empty
# input: pr digit/list for prefix, m middle FingerTree of nodes, sf suffix digit/list.
# output: a valid FingerTree preserving order; if pr is empty it borrows from m or
# collapses to a tree built from sf.
deepL(pr, m, sf, monoids) %::% . : FingerTree : . : list : FingerTree
deepL(pr, m, sf, monoids) %as% {
  if(length(pr) > 0) {
    return(build_deep(pr, m, sf, monoids))
  }
  if(m %isa% Empty) {
    return(digit_to_tree(sf, monoids))
  }
  res <- viewL(m, monoids)
  node <- res$elem
  m_rest <- res$rest
  new_pr <- node_to_digit(node, monoids)
  build_deep(new_pr, m_rest, sf, monoids)
}

# deepR: rebuild Deep, possibly pulling from middle if suffix is empty
# input: pr prefix digit/list, m middle FingerTree of nodes, sf suffix digit/list.
# output: a valid FingerTree preserving order; if sf is empty it borrows from m or
# collapses to a tree built from pr.
deepR(pr, m, sf, monoids) %::% . : FingerTree : . : list : FingerTree
deepR(pr, m, sf, monoids) %as% {
  if(length(sf) > 0) {
    return(build_deep(pr, m, sf, monoids))
  }
  if(m %isa% Empty) {
    return(digit_to_tree(pr, monoids))
  }
  res <- viewR(m, monoids)
  node <- res$elem
  m_rest <- res$rest
  new_sf <- node_to_digit(node, monoids)
  build_deep(pr, m_rest, new_sf, monoids)
}
