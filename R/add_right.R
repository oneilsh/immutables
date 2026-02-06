
add_right(e, el) %::% Empty : . : Single
add_right(e, el) %as% {
  Single(el)
}



add_right(s, el) %::% Single : . : Deep
add_right(s, el) %as% {
  Deep(Digit(s[[1]]), Empty(), Digit(el))
}


add_right(d, el) %::% Digit : . : Digit
add_right(d, el) %as% {
  oldclasses <- class(d)
  oldid <- attr(d, "id")
  newd <- list.append(d, el)
  class(newd) <- oldclasses
  attr(newd, "id") <- oldid
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




add_all_right(t, els) %::% FingerTree : . : FingerTree
add_all_right(t, els) %as% {
  for(el in els) {
    t <- add_right(t, el)
  }
  return(t)
}

