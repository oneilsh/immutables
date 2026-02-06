

# reduce_left and reduce_right for empties and singles; these require utilizing the reducers identity element
reduce_left(e, r) %::% Empty : Reducer : .   # if it's an empty tree...
reduce_left(e, r) %as% r$i    # it's just the identity



reduce_left(s, r) %::% Single : Reducer : .   # if it's a single element...
reduce_left(s, r) %as% {
  reduce_left(s[[1]], r)
}

reduce_left(e, r) %::% Element : Reducer : . # an element can be reduced too
reduce_left(e, r) %as% r$f(r$i, e)



reduce_left(n, r) %::% Node : Reducer: .
reduce_left(n, r) %as% {
  curr <- r$i
  for(el in n) {
    el_reduced <- reduce_left(el, r)
    curr <- r$f(curr, el_reduced)
  }
  return(curr)
}


reduce_left(d, r) %::% Digit : Reducer : .
reduce_left(d, r) %as% {
  curr <- r$i
  for(el in d) {
    el_reduced <- reduce_left(el, r)
    curr <- r$f(curr, el_reduced)
  }
  return(curr)
}



# reduce_left for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_left(t, r) %::% Deep : Reducer : .
reduce_left(t, r) %as% {
  prefix_reduced <- reduce_left(t$prefix, r)
  middle_reduced <- reduce_left(t$middle, r)
  suffix_reduced <- reduce_left(t$suffix, r)
  answer <- reduce_left(Digit(prefix_reduced, middle_reduced, suffix_reduced), r)
  return(answer)
}
