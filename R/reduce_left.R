

# monoid fold helper; folds left over a tree or node structure
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
reduce_left_impl(e, r) %::% Empty : MeasureMonoid : .
reduce_left_impl(e, r) %as% r$i



reduce_left_impl(s, r) %::% Single : MeasureMonoid : .
reduce_left_impl(s, r) %as% {
  reduce_left_acc(s, r, r$i)
}

# legacy Element wrapper (elements now can be any type)


reduce_left_impl(n, r) %::% Node : MeasureMonoid : .
reduce_left_impl(n, r) %as% {
  reduce_left_acc(n, r, r$i)
}

reduce_left_impl(d, r) %::% Digit : MeasureMonoid : .
reduce_left_impl(d, r) %as% {
  reduce_left_acc(d, r, r$i)
}


# reduce_left_impl for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_left_impl(t, r) %::% Deep : MeasureMonoid : .
reduce_left_impl(t, r) %as% {
  reduce_left_acc(t, r, r$i)
}
