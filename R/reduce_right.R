

reduce_right_acc <- function(t, r, acc) {
  if(is_structural_node(t) && t %isa% Empty) {
    return(acc)
  }
  if(!is_structural_node(t)) {
    return(r$f(t, acc))
  }
  if(is_structural_node(t) && t %isa% Single) {
    return(reduce_right_acc(t[[1]], r, acc))
  }
  if(is_structural_node(t) && t %isa% Deep) {
    acc <- reduce_right_acc(t$suffix, r, acc)
    acc <- reduce_right_acc(t$middle, r, acc)
    acc <- reduce_right_acc(t$prefix, r, acc)
    return(acc)
  }
  for(el in rev(t)) {
    acc <- reduce_right_acc(el, r, acc)
  }
  return(acc)
}

reduce_right(e, r) %::% Empty : Reducer : .   # if it's an empty tree...
reduce_right(e, r) %as% r$i    # it's just the identity



reduce_right(s, r) %::% Single : Reducer : .   # if it's a single element...
reduce_right(s, r) %as% {
  reduce_right_acc(s, r, r$i)
}


# legacy Element wrapper (elements now can be any type)

reduce_right(n, r) %::% Node : Reducer: .
reduce_right(n, r) %as% {
  reduce_right_acc(n, r, r$i)
}



# reduce_right for digits, which can have 1 to 4 elements; again we just call the reducer function with the right grouping
reduce_right(d, r) %::% Digit : Reducer : .
reduce_right(d, r) %as% {
  reduce_right_acc(d, r, r$i)
}


# reduce_right for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_right(t, r) %::% Deep : Reducer : .
reduce_right(t, r) %as% {
  reduce_right_acc(t, r, r$i)
}
