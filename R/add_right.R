add_right(e, el, monoids) %::% Empty : . : list : Single
add_right(e, el, monoids) %as% {
  measured_single(el, monoids)
}


add_right(s, el, monoids) %::% Single : . : list : Deep
add_right(s, el, monoids) %as% {
  measured_deep(
    measured_digit(.subset2(s, 1), monoids = monoids),
    measured_empty(monoids),
    measured_digit(el, monoids = monoids),
    monoids
  )
}

add_right(d, el, monoids) %::% Digit : . : list : Digit
add_right(d, el, monoids) %as% {
  oldclasses <- class(d)
  newd <- list.append(d, el)
  class(newd) <- oldclasses
  set_measure(newd, monoids)
}




# symmetric case for add_right. Only new nodes get measures.
add_right(d, el, monoids) %::% Deep : . : list : Deep
add_right(d, el, monoids) %as% {
  if(length(d$suffix) == 4) {
    new_suffix <- measured_digit(d$suffix[[4]], el, monoids = monoids)
    new_middle_node <- measured_node3(d$suffix[[1]], d$suffix[[2]], d$suffix[[3]], monoids)
    new_middle <- add_right(d$middle, new_middle_node, monoids)
    measured_deep(prefix = d$prefix, middle = new_middle, suffix = new_suffix, monoids)
  } else {
    new_suffix <- add_right(d$suffix, el, monoids)
    measured_deep(prefix = d$prefix, middle = d$middle, suffix = new_suffix, monoids)
  }
}


add_all_right(t, els, monoids) %::% FingerTree : . : list : FingerTree
add_all_right(t, els, monoids) %as% {
  for(el in els) {
    t <- add_right(t, el, monoids)
  }
  return(t)
}
