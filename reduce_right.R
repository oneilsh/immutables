

reduce_right(e, r) %::% Empty : Reducer : .   # if it's an empty tree...
reduce_right(e, r) %as% r$i    # it's just the identity



reduce_right(s, r) %::% Single : Reducer : .   # if it's a single element...
reduce_right(s, r) %as% {
  el_reduced <- reduce_right(s[[1]], r)
  r$f(r$i, el_reduced)    # it's just the identity with that element, identity first for left
}

reduce_right(e, r) %::% Element : Reducer : . # an element can be reduced too
reduce_right(e, r) %as% r$f(r$i, e)


reduce_right(n, r) %::% Node : Reducer: .
reduce_right(n, r) %as% {
  cache_value <- paste0(as.character(r), collapse = "")
  if(!is.null(n@cache[[cache_value]])) { return(n@cache[[cache_value]]) }
  curr <- r$i
  for(el in rev(n)) {
    el_reduced <- reduce_right(el, r)
    curr <- r$f(el_reduced, curr)
  }
  
  n@cache[[cache_value]] <- curr
  return(curr)
}



# reduce_right for digits, which can have 1 to 4 elements; again we just call the reducer function with the right grouping
reduce_right(d, r) %::% Digit : Reducer : .
reduce_right(d, r) %as% {
  cache_value <- paste0(as.character(r), collapse = "")
  if(!is.null(d@cache[[cache_value]])) { return(d@cache[[cache_value]]) }
  
  curr <- r$i
  for(el in rev(d)) {
    el_reduced <- reduce_right(el, r)
    curr <- r$f(el_reduced, curr)
  }
  
  d@cache[[cache_value]] <- curr
  return(curr)
}


# reduce_right for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_right(t, r) %::% Deep : Reducer : .
reduce_right(t, r) %as% {
  cache_value <- paste0(as.character(r), collapse = "")
  if(!is.null(t@cache[[cache_value]])) { return(t@cache[[cache_value]]) }
  
  prefix_reduced <- reduce_right(t$prefix, r)
  middle_reduced <- reduce_right(t$middle, r)
  suffix_reduced <- reduce_right(t$suffix, r)
  
  answer <- reduce_right(Digit(prefix_reduced, middle_reduced, suffix_reduced), r)
  t@cache[[cache_value]] <- answer
  return(answer)
}

