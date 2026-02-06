
add_left(e, el) %::% Empty : . : Single
add_left(e, el) %as% {
  Single(el)
}

add_left(e, el, r) %::% Empty : . : MeasuredReducer : Single
add_left(e, el, r) %as% {
  measured_single(el, r)
}

add_left(s, el) %::% Single : . : Deep
add_left(s, el) %as% {
  Deep(Digit(el), Empty(), Digit(s[[1]]))
}

add_left(s, el, r) %::% Single : . : MeasuredReducer : Deep
add_left(s, el, r) %as% {
  measured_deep(
    measured_digit(el, r = r),
    measured_empty(r),
    measured_digit(s[[1]], r = r),
    r
  )
}

# this is actually a pain, because we implement digits as lists with various attributes
# we can't just c() the element to the list, as we'd get a list(el, old_list), rather than list(el, old_list[[1]], old_list[[2]]), etc.
# list.prepend from the rlist package takes care of that.
# BUT, it strips off the attributes, including class and our random id. So we grab em, prepend, reset em.
add_left(d, el) %::% Digit : . : Digit
add_left(d, el) %as% {
  oldclasses <- class(d)
  newd <- list.prepend(d, el)
  class(newd) <- oldclasses
  return(newd)
}

add_left(d, el, r) %::% Digit : . : MeasuredReducer : Digit
add_left(d, el, r) %as% {
  oldclasses <- class(d)
  newd <- list.prepend(d, el)
  class(newd) <- oldclasses
  attr(newd, "measure") <- measure_child(newd, r)
  return(newd)
}



# this is where the magic happens; 
# if the prefix digit has 4 elements: then we push off the last to 
# be stored deeper along inside a Node3, and just store the new el and the remaining 1 from the digit in the prefix as a digit.
# otherwise: it's a simple add to the prefix digit
add_left(d, el) %::% Deep : . : Deep
add_left(d, el) %as% {
  if(length(d$prefix) == 4) {
    new_prefix <- Digit(el, d$prefix[[1]])
    new_middle_node <- Node3(d$prefix[[2]], d$prefix[[3]], d$prefix[[4]])
    new_middle <- add_left(d$middle, new_middle_node)
    return(Deep(prefix = new_prefix, middle = new_middle, suffix = d$suffix))
  } else {
    new_prefix <- add_left(d$prefix, el)
    return(Deep(prefix = new_prefix, middle = d$middle, suffix = d$suffix))
  }
}

add_left(d, el, r) %::% Deep : . : MeasuredReducer : Deep
add_left(d, el, r) %as% {
  if(length(d$prefix) == 4) {
    new_prefix <- measured_digit(el, d$prefix[[1]], r = r)
    new_middle_node <- measured_node3(d$prefix[[2]], d$prefix[[3]], d$prefix[[4]], r)
    new_middle <- add_left(d$middle, new_middle_node, r)
    measured_deep(prefix = new_prefix, middle = new_middle, suffix = d$suffix, r)
  } else {
    new_prefix <- add_left(d$prefix, el, r)
    measured_deep(prefix = new_prefix, middle = d$middle, suffix = d$suffix, r)
  }
}


add_all_left(t, els) %::% FingerTree : . : FingerTree
add_all_left(t, els) %as% {
  for(el in rev(els)) {
    t <- add_left(t, el)
  }
  return(t)
}

add_all_left(t, els, r) %::% FingerTree : . : MeasuredReducer : FingerTree
add_all_left(t, els, r) %as% {
  for(el in rev(els)) {
    t <- add_left(t, el, r)
  }
  return(t)
}
