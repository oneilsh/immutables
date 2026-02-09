

# reducer fold helper; folds left over a tree or node structure
reduce_left_acc(t, r, acc) %::% . : . : . : .
reduce_left_acc(t, r, acc) %as% {
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

# reduce_left_impl methods for different node types; uses identity once at top
reduce_left_impl(e, r) %::% Empty : Reducer : .   # if it's an empty tree...
reduce_left_impl(e, r) %as% r$i    # it's just the identity

reduce_left_impl(e, r) %::% Empty : MeasureReducer : .
reduce_left_impl(e, r) %as% r$i



reduce_left_impl(s, r) %::% Single : Reducer : .   # if it's a single element...
reduce_left_impl(s, r) %as% {
  reduce_left_acc(s, r, r$i)
}

reduce_left_impl(s, r) %::% Single : MeasureReducer : .
reduce_left_impl(s, r) %as% {
  reduce_left_acc(s, r, r$i)
}

# legacy Element wrapper (elements now can be any type)


reduce_left_impl(n, r) %::% Node : Reducer: .
reduce_left_impl(n, r) %as% {
  reduce_left_acc(n, r, r$i)
}

reduce_left_impl(n, r) %::% Node : MeasureReducer : .
reduce_left_impl(n, r) %as% {
  reduce_left_acc(n, r, r$i)
}

reduce_left_impl(d, r) %::% Digit : Reducer : .
reduce_left_impl(d, r) %as% {
  reduce_left_acc(d, r, r$i)
}

reduce_left_impl(d, r) %::% Digit : MeasureReducer : .
reduce_left_impl(d, r) %as% {
  reduce_left_acc(d, r, r$i)
}


# reduce_left_impl for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_left_impl(t, r) %::% Deep : Reducer : .
reduce_left_impl(t, r) %as% {
  reduce_left_acc(t, r, r$i)
}

reduce_left_impl(t, r) %::% Deep : MeasureReducer : .
reduce_left_impl(t, r) %as% {
  reduce_left_acc(t, r, r$i)
}
