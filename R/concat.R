##
## app3 is what the original paper called this generalized concatenation function used by concat()
##




## concatenation, depending on what kinds of finger trees we want to concatenate we do different things
# each concat function also takes a list of elements to smush between the two trees, useful for later functionality
# generalized concatenation helper; inserts list ts between two trees
app3(e, ts, xs, monoids) %::% Empty : list : FingerTree : list : FingerTree
app3(e, ts, xs, monoids) %as% add_all_left(xs, ts, monoids)

app3(xs, ts, e, monoids) %::% FingerTree : list : Empty : list : FingerTree
app3(xs, ts, e, monoids) %as% add_all_right(xs, ts, monoids)

app3(x, ts, xs, monoids) %::% Single : list : FingerTree : list : FingerTree
app3(x, ts, xs, monoids) %as% add_left(add_all_left(xs, ts, monoids), x, monoids)

app3(xs, ts, x, monoids) %::% FingerTree : list : Single : list : FingerTree
app3(xs, ts, x, monoids) %as% add_right(add_all_right(xs, ts, monoids), x, monoids)

app3(xs, ts, ys, monoids) %::% Deep : list : Deep : list : FingerTree
app3(xs, ts, ys, monoids) %as% {
  measured_deep(
    .subset2(xs,"prefix"),
    app3(.subset2(xs,"middle"),
         measured_nodes(c(.subset2(xs,"suffix"), ts, .subset2(ys,"prefix")), monoids),
         .subset2(ys,"middle"),
         monoids),
    .subset2(ys,"suffix"),
    monoids
  )
}

concat(xs, ys, monoids) %::% FingerTree : FingerTree : list : FingerTree
concat(xs, ys, monoids) %as% {
  app3(xs, list(), ys, monoids)
}
