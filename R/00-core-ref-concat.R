##
## app3 is what the original paper called this generalized concatenation function used by concat()
##

## concatenation, depending on what kinds of finger trees we want to concatenate we do different things
# each concat function also takes a list of elements to smush between the two trees, useful for later functionality
# generalized concatenation helper; inserts list ts between two trees
# Runtime: see `.app3_fast`.
app3(e, ts, xs, monoids) %::% Empty : list : FingerTree : list : FingerTree
app3(e, ts, xs, monoids) %as% .app3_fast(e, ts, xs, monoids)

# Runtime: see `.app3_fast`.
app3(xs, ts, e, monoids) %::% FingerTree : list : Empty : list : FingerTree
app3(xs, ts, e, monoids) %as% .app3_fast(xs, ts, e, monoids)

# Runtime: see `.app3_fast`.
app3(x, ts, xs, monoids) %::% Single : list : FingerTree : list : FingerTree
app3(x, ts, xs, monoids) %as% .app3_fast(x, ts, xs, monoids)

# Runtime: see `.app3_fast`.
app3(xs, ts, x, monoids) %::% FingerTree : list : Single : list : FingerTree
app3(xs, ts, x, monoids) %as% .app3_fast(xs, ts, x, monoids)

# Runtime: see `.app3_fast`.
app3(xs, ts, ys, monoids) %::% Deep : list : Deep : list : FingerTree
app3(xs, ts, ys, monoids) %as% .app3_fast(xs, ts, ys, monoids)

# Runtime: O(log(min(nx, ny))) for balanced trees, where nx/ny are input sizes.
concat(xs, ys, monoids) %::% FingerTree : FingerTree : list : FingerTree
concat(xs, ys, monoids) %as% {
  .app3_fast(xs, list(), ys, monoids)
}

# convert a flat list into measured Node2/Node3 list for concatenation.
# Runtime: O(k), where k = length(l).
measured_nodes(l, monoids) %::% list : list : list
measured_nodes(l, monoids) %as% {
  if(length(l) == 2) { return(list(
    measured_node2( l[[1]], l[[2]], monoids )
  ))}
  if(length(l) == 3) { return(list(
    measured_node3( l[[1]], l[[2]], l[[3]], monoids )
  ))}
  if(length(l) == 4) { return(list(
    measured_node2( l[[1]], l[[2]], monoids ),
    measured_node2( l[[3]], l[[4]], monoids )
  ))}

  first = measured_node3( l[[1]], l[[2]], l[[3]], monoids )
  rest = measured_nodes(l[4:length(l)], monoids)
  rest = c(list(first), rest)
  return(rest)
}
