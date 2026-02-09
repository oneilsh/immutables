# core split implementation following Hinze/Paterson 4.4
#
# split_tree_impl returns a distinguished element with left/right context:
#   list(left = FingerTree, elem = element, right = FingerTree)
# It assumes:
# - t is non-empty
# - p(i) is FALSE and p(i <> measure(t)) is TRUE for strict split semantics
# The public wrappers relax these preconditions (matching the paper's split).
split_tree_impl(p, i, t, mr) %::% Function : . : FingerTree : MeasureReducer : list
split_tree_impl(p, i, t, mr) %as% {
  if(t %isa% Empty) {
    stop("split_tree_impl requires a non-empty tree")
  }

  if(t %isa% Single) {
    return(list(
      left = measured_empty(mr),
      elem = t[[1]],
      right = measured_empty(mr)
    ))
  }

  # Deep(pr, m, sf): test where predicate first flips using cached measures.
  vpr <- mr$f(i, measure_child(t$prefix, mr))
  vm <- mr$f(vpr, measure_child(t$middle, mr))

  if(p(vpr)) {
    # split occurs in prefix digit
    s <- split_digit(p, i, t$prefix, mr)
    left_tree <- digit_to_tree(s$left, mr)
    right_tree <- deepL(build_digit(s$right, mr), t$middle, t$suffix, mr)
    return(list(left = left_tree, elem = s$elem, right = right_tree))
  }

  if(p(vm)) {
    # split occurs in middle tree, then inside the selected Node2/Node3
    sm <- split_tree_impl(p, vpr, t$middle, mr)
    inode <- mr$f(vpr, measure_child(sm$left, mr))
    sx <- split_digit(p, inode, as.list(sm$elem), mr)
    left_tree <- deepR(t$prefix, sm$left, build_digit(sx$left, mr), mr)
    right_tree <- deepL(build_digit(sx$right, mr), sm$right, t$suffix, mr)
    return(list(left = left_tree, elem = sx$elem, right = right_tree))
  }

  # split occurs in suffix digit
  s <- split_digit(p, vm, t$suffix, mr)
  left_tree <- deepR(t$prefix, t$middle, build_digit(s$left, mr), mr)
  right_tree <- digit_to_tree(s$right, mr)
  list(left = left_tree, elem = s$elem, right = right_tree)
}
