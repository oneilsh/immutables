

reduce_right(e, r) %::% Empty : Reducer : .   # if it's an empty tree...
reduce_right(e, r) %as% r$i    # it's just the identity



reduce_right(s, r) %::% Single : Reducer : .   # if it's a single element...
reduce_right(s, r) %as% {
  reduce_right(s[[1]], r)
}

reduce_right(e, r) %::% Element : Reducer : . # an element can be reduced too
reduce_right(e, r) %as% r$f(e, r$i)


reduce_right(n, r) %::% Node : Reducer: .
reduce_right(n, r) %as% {
  curr <- r$i
  for(el in rev(n)) {
    el_reduced <- reduce_right(el, r)
    curr <- r$f(el_reduced, curr)
  }
  return(curr)
}



# reduce_right for digits, which can have 1 to 4 elements; again we just call the reducer function with the right grouping
reduce_right(d, r) %::% Digit : Reducer : .
reduce_right(d, r) %as% {
  curr <- r$i
  for(el in rev(d)) {
    el_reduced <- reduce_right(el, r)
    curr <- r$f(el_reduced, curr)
  }
  return(curr)
}


# reduce_right for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_right(t, r) %::% Deep : Reducer : .
reduce_right(t, r) %as% {
  prefix_reduced <- reduce_right(t$prefix, r)
  middle_reduced <- reduce_right(t$middle, r)
  suffix_reduced <- reduce_right(t$suffix, r)
  
  answer <- reduce_right(Digit(prefix_reduced, middle_reduced, suffix_reduced), r)
  return(answer)
}
