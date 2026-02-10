##
## app3 is what the original paper called this generalized concatenation function used by concat()
##




## concatenation, depending on what kinds of finger trees we want to concatenate we do different things
# each concat function also takes a list of elements to smush between the two trees, useful for later functionality
# generalized concatenation helper; inserts list ts between two trees
app3(e, ts, xs, r) %::% Empty : list : FingerTree : MeasureMonoid : FingerTree
app3(e, ts, xs, r) %as% add_all_left(xs, ts, r)

app3(xs, ts, e, r) %::% FingerTree : list : Empty : MeasureMonoid : FingerTree
app3(xs, ts, e, r) %as% add_all_right(xs, ts, r)

app3(x, ts, xs, r) %::% Single : list : FingerTree : MeasureMonoid : FingerTree
app3(x, ts, xs, r) %as% add_left(add_all_left(xs, ts, r), x, r)

app3(xs, ts, x, r) %::% FingerTree : list : Single : MeasureMonoid : FingerTree
app3(xs, ts, x, r) %as% add_right(add_all_right(xs, ts, r), x, r)

app3(xs, ts, ys, r) %::% Deep : list : Deep : MeasureMonoid : FingerTree
app3(xs, ts, ys, r) %as% {
  measured_deep(
    xs$prefix,
    app3(xs$middle,
         measured_nodes(c(xs$suffix, ts, ys$prefix), r),
         ys$middle,
         r),
    ys$suffix,
    r
  )
}

concat(xs, ys, r) %::% FingerTree : FingerTree : MeasureMonoid : FingerTree
concat(xs, ys, r) %as% {
  app3(xs, list(), ys, r)
}
