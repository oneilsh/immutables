

# reducer fold helper; folds right over a tree or node structure
reduce_right_acc(t, r, acc) %::% . : . : . : .
reduce_right_acc(t, r, acc) %as% {
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

# reduce_right_impl methods for different node types; uses identity once at top
reduce_right_impl(e, r) %::% Empty : Reducer : .   # if it's an empty tree...
reduce_right_impl(e, r) %as% r$i    # it's just the identity

reduce_right_impl(e, r) %::% Empty : MeasureReducer : .
reduce_right_impl(e, r) %as% r$i



reduce_right_impl(s, r) %::% Single : Reducer : .   # if it's a single element...
reduce_right_impl(s, r) %as% {
  reduce_right_acc(s, r, r$i)
}

reduce_right_impl(s, r) %::% Single : MeasureReducer : .
reduce_right_impl(s, r) %as% {
  reduce_right_acc(s, r, r$i)
}

# legacy Element wrapper (elements now can be any type)

reduce_right_impl(n, r) %::% Node : Reducer: .
reduce_right_impl(n, r) %as% {
  reduce_right_acc(n, r, r$i)
}

reduce_right_impl(n, r) %::% Node : MeasureReducer : .
reduce_right_impl(n, r) %as% {
  reduce_right_acc(n, r, r$i)
}


# reduce_right_impl for digits, which can have 1 to 4 elements; again we just call the reducer function with the right grouping
reduce_right_impl(d, r) %::% Digit : Reducer : .
reduce_right_impl(d, r) %as% {
  reduce_right_acc(d, r, r$i)
}

reduce_right_impl(d, r) %::% Digit : MeasureReducer : .
reduce_right_impl(d, r) %as% {
  reduce_right_acc(d, r, r$i)
}

# reduce_right_impl for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_right_impl(t, r) %::% Deep : Reducer : .
reduce_right_impl(t, r) %as% {
  reduce_right_acc(t, r, r$i)
}

reduce_right_impl(t, r) %::% Deep : MeasureReducer : .
reduce_right_impl(t, r) %as% {
  reduce_right_acc(t, r, r$i)
}
