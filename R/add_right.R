
add_right(e, el) %::% Empty : . : Single
add_right(e, el) %as% {
  Single(el)
}

# measured overload: returns a measured Single
add_right(e, el, r) %::% Empty : . : MeasureMonoid : Single
add_right(e, el, r) %as% {
  measured_single(el, r)
}


add_right(s, el) %::% Single : . : Deep
add_right(s, el) %as% {
  Deep(Digit(s[[1]]), Empty(), Digit(el))
}

# measured overload: builds a measured Deep from a Single
add_right(s, el, r) %::% Single : . : MeasureMonoid : Deep
add_right(s, el, r) %as% {
  measured_deep(
    measured_digit(s[[1]], r = r),
    measured_empty(r),
    measured_digit(el, r = r),
    r
  )
}

add_right(d, el) %::% Digit : . : Digit
add_right(d, el) %as% {
  oldclasses <- class(d)
  newd <- list.append(d, el)
  class(newd) <- oldclasses
  return(newd)
}

# measured overload: updates digit measure after append
add_right(d, el, r) %::% Digit : . : MeasureMonoid : Digit
add_right(d, el, r) %as% {
  oldclasses <- class(d)
  newd <- list.append(d, el)
  class(newd) <- oldclasses
  attr(newd, "measure") <- measure_child(newd, r)
  return(newd)
}




# symmetric case for add_right
add_right(d, el) %::% Deep : . : Deep
add_right(d, el) %as% {
  if(length(d$suffix) == 4) {
    new_suffix <- Digit(d$suffix[[4]], el)
    new_middle_node <- Node3(d$suffix[[1]], d$suffix[[2]], d$suffix[[3]])
    new_middle <- add_right(d$middle, new_middle_node)
    return(Deep(prefix = d$prefix, middle = new_middle, suffix = new_suffix))
  } else {
    new_suffix <- add_right(d$suffix, el)
    return(Deep(prefix = d$prefix, middle = d$middle, suffix = new_suffix))
  }
}

# measured overload: only new nodes get measures
add_right(d, el, r) %::% Deep : . : MeasureMonoid : Deep
add_right(d, el, r) %as% {
  if(length(d$suffix) == 4) {
    new_suffix <- measured_digit(d$suffix[[4]], el, r = r)
    new_middle_node <- measured_node3(d$suffix[[1]], d$suffix[[2]], d$suffix[[3]], r)
    new_middle <- add_right(d$middle, new_middle_node, r)
    measured_deep(prefix = d$prefix, middle = new_middle, suffix = new_suffix, r)
  } else {
    new_suffix <- add_right(d$suffix, el, r)
    measured_deep(prefix = d$prefix, middle = d$middle, suffix = new_suffix, r)
  }
}



add_all_right(t, els) %::% FingerTree : . : FingerTree
add_all_right(t, els) %as% {
  for(el in els) {
    t <- add_right(t, el)
  }
  return(t)
}

add_all_right(t, els, r) %::% FingerTree : . : MeasureMonoid : FingerTree
add_all_right(t, els, r) %as% {
  for(el in els) {
    t <- add_right(t, el, r)
  }
  return(t)
}
