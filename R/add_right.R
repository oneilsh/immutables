add_right(e, el, r) %::% Empty : . : MeasureMonoid : Single
add_right(e, el, r) %as% {
  measured_single(el, r)
}


add_right(s, el, r) %::% Single : . : MeasureMonoid : Deep
add_right(s, el, r) %as% {
  measured_deep(
    measured_digit(s[[1]], r = r),
    measured_empty(r),
    measured_digit(el, r = r),
    r
  )
}

add_right(d, el, r) %::% Digit : . : MeasureMonoid : Digit
add_right(d, el, r) %as% {
  oldclasses <- class(d)
  newd <- list.append(d, el)
  class(newd) <- oldclasses
  set_measure(newd, r)
}




# symmetric case for add_right. Only new nodes get measures.
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


add_all_right(t, els, r) %::% FingerTree : . : MeasureMonoid : FingerTree
add_all_right(t, els, r) %as% {
  for(el in els) {
    t <- add_right(t, el, r)
  }
  return(t)
}
