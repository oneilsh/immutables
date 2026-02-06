

reduce_left_acc <- function(t, r, acc) {
  if(is_structural_node(t) && t %isa% Empty) {
    return(acc)
  }
  if(!is_structural_node(t)) {
    return(r$f(acc, t))
  }
  if(is_structural_node(t) && t %isa% Single) {
    return(reduce_left_acc(t[[1]], r, acc))
  }
  if(is_structural_node(t) && t %isa% Deep) {
    acc <- reduce_left_acc(t$prefix, r, acc)
    acc <- reduce_left_acc(t$middle, r, acc)
    acc <- reduce_left_acc(t$suffix, r, acc)
    return(acc)
  }
  for(el in t) {
    acc <- reduce_left_acc(el, r, acc)
  }
  return(acc)
}

# reduce_left and reduce_right for empties and singles; these require utilizing the reducers identity element
reduce_left(e, r) %::% Empty : Reducer : .   # if it's an empty tree...
reduce_left(e, r) %as% r$i    # it's just the identity



reduce_left(s, r) %::% Single : Reducer : .   # if it's a single element...
reduce_left(s, r) %as% {
  reduce_left_acc(s, r, r$i)
}


# legacy Element wrapper (elements now can be any type)


reduce_left(n, r) %::% Node : Reducer: .
reduce_left(n, r) %as% {
  reduce_left_acc(n, r, r$i)
}


reduce_left(d, r) %::% Digit : Reducer : .
reduce_left(d, r) %as% {
  reduce_left_acc(d, r, r$i)
}



# reduce_left for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_left(t, r) %::% Deep : Reducer : .
reduce_left(t, r) %as% {
  reduce_left_acc(t, r, r$i)
}
