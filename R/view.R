# helpers for converting between digits/nodes/trees and for deconstructing trees

# construct a Digit from a list of elements.
# input: xs list of 1..4 elements (raw elements or nodes), optional measure monoid r.
# output: Digit(xs...) with preserved order, measured if r is a MeasureMonoid.
# note: empty input returns an empty list() sentinel, used by deepL/deepR rebuild logic.
build_digit(xs) %::% list : .
build_digit(xs) %as% {
  if(length(xs) == 0) {
    return(list())
  }
  do.call(Digit, xs)
}

build_digit(xs, r) %::% list : MeasureMonoid : .
build_digit(xs, r) %as% {
  if(length(xs) == 0) {
    return(list())
  }
  do.call(measured_digit, c(xs, list(r = r)))
}

# build a Deep node from prefix digit, middle tree, and suffix digit.
# input: pr (Digit), m (FingerTree), sf (Digit), optional measure monoid r.
# output: Deep(pr, m, sf), measured if r is a MeasureMonoid.
build_deep(pr, m, sf) %::% Digit : FingerTree : Digit : Deep
build_deep(pr, m, sf) %as% Deep(pr, m, sf)

build_deep(pr, m, sf, r) %::% Digit : FingerTree : Digit : MeasureMonoid : Deep
build_deep(pr, m, sf, r) %as% measured_deep(pr, m, sf, r)

# convert a small list/digit (size 0..4) into a valid FingerTree shape.
# input: d list of elements (typically a Digit), optional measure monoid r.
# output: Empty / Single / Deep representing the same left-to-right sequence.
digit_to_tree(d) %::% list : FingerTree
digit_to_tree(d) %as% {
  n <- length(d)
  if(n == 0) { return(Empty()) }
  if(n == 1) { return(Single(d[[1]])) }
  if(n == 2) {
    pr <- build_digit(list(d[[1]]))
    sf <- build_digit(list(d[[2]]))
    return(build_deep(pr, Empty(), sf))
  }
  if(n == 3) {
    pr <- build_digit(list(d[[1]], d[[2]]))
    sf <- build_digit(list(d[[3]]))
    return(build_deep(pr, Empty(), sf))
  }
  if(n == 4) {
    pr <- build_digit(list(d[[1]], d[[2]]))
    sf <- build_digit(list(d[[3]], d[[4]]))
    return(build_deep(pr, Empty(), sf))
  }
  stop("digit_to_tree expects a digit of size 0..4")
}

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

# convert a Node2/Node3 into a Digit of its children.
# input: node (Node2 or Node3), optional measure monoid r.
# output: Digit with 2 or 3 elements in the same order.
node_to_digit(node) %::% Node : Digit
node_to_digit(node) %as% build_digit(as.list(node))

node_to_digit(node, r) %::% Node : MeasureMonoid : Digit
node_to_digit(node, r) %as% build_digit(as.list(node), r)

# viewL: return leftmost element and the remaining tree
# input: t non-empty FingerTree, optional measure monoid r.
# output: list(elem = leftmost element of t, rest = t without that element).
viewL(t) %::% FingerTree : list
viewL(t) %as% {
  if(t %isa% Empty) {
    stop("viewL on Empty")
  }
  if(t %isa% Single) {
    return(list(elem = t[[1]], rest = Empty()))
  }
  pr <- t$prefix
  if(length(pr) > 1) {
    head <- pr[[1]]
    tail <- pr[2:length(pr)]
    new_pr <- build_digit(tail)
    return(list(elem = head, rest = build_deep(new_pr, t$middle, t$suffix)))
  }
  head <- pr[[1]]
  m <- t$middle
  if(m %isa% Empty) {
    return(list(elem = head, rest = digit_to_tree(t$suffix)))
  }
  res <- viewL(m)
  node <- res$elem
  m_rest <- res$rest
  new_pr <- node_to_digit(node)
  list(elem = head, rest = build_deep(new_pr, m_rest, t$suffix))
}

viewL(t, r) %::% FingerTree : MeasureMonoid : list
viewL(t, r) %as% {
  if(t %isa% Empty) {
    stop("viewL on Empty")
  }
  if(t %isa% Single) {
    return(list(elem = t[[1]], rest = measured_empty(r)))
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
# input: t non-empty FingerTree, optional measure monoid r.
# output: list(elem = rightmost element of t, rest = t without that element).
viewR(t) %::% FingerTree : list
viewR(t) %as% {
  if(t %isa% Empty) {
    stop("viewR on Empty")
  }
  if(t %isa% Single) {
    return(list(elem = t[[1]], rest = Empty()))
  }
  sf <- t$suffix
  if(length(sf) > 1) {
    head <- sf[[length(sf)]]
    tail <- sf[1:(length(sf) - 1)]
    new_sf <- build_digit(tail)
    return(list(elem = head, rest = build_deep(t$prefix, t$middle, new_sf)))
  }
  head <- sf[[1]]
  m <- t$middle
  if(m %isa% Empty) {
    return(list(elem = head, rest = digit_to_tree(t$prefix)))
  }
  res <- viewR(m)
  node <- res$elem
  m_rest <- res$rest
  new_sf <- node_to_digit(node)
  list(elem = head, rest = build_deep(t$prefix, m_rest, new_sf))
}

viewR(t, r) %::% FingerTree : MeasureMonoid : list
viewR(t, r) %as% {
  if(t %isa% Empty) {
    stop("viewR on Empty")
  }
  if(t %isa% Single) {
    return(list(elem = t[[1]], rest = measured_empty(r)))
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
deepL(pr, m, sf) %::% . : FingerTree : . : FingerTree
deepL(pr, m, sf) %as% {
  if(length(pr) > 0) {
    return(build_deep(pr, m, sf))
  }
  if(m %isa% Empty) {
    return(digit_to_tree(sf))
  }
  res <- viewL(m)
  node <- res$elem
  m_rest <- res$rest
  new_pr <- node_to_digit(node)
  build_deep(new_pr, m_rest, sf)
}

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
deepR(pr, m, sf) %::% . : FingerTree : . : FingerTree
deepR(pr, m, sf) %as% {
  if(length(sf) > 0) {
    return(build_deep(pr, m, sf))
  }
  if(m %isa% Empty) {
    return(digit_to_tree(pr))
  }
  res <- viewR(m)
  node <- res$elem
  m_rest <- res$rest
  new_sf <- node_to_digit(node)
  build_deep(pr, m_rest, new_sf)
}

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
