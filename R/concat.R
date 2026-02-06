##
## app3 is what the original paper called this generalized concatenation function used by concat()
##




## concatenation, depending on what kinds of finger trees we want to concatenate we do different things
# each concat function also takes a list of elements to smush between the two trees, useful for later functionality
app3(e, ts, xs) %::% Empty : list : FingerTree : FingerTree
app3(e, ts, xs) %as% add_all_left(xs, ts)

app3(xs, ts, e) %::% FingerTree : list : Empty : FingerTree
app3(xs, ts, e) %as% add_all_right(xs, ts)

app3(x, ts, xs) %::% Single : list : FingerTree : FingerTree
app3(x, ts, xs) %as% add_left(add_all_left(xs, ts), x)

app3(xs, ts, x) %::% FingerTree : list : Single : FingerTree
app3(xs, ts, x) %as% add_right(add_all_right(xs, ts), x)

# the toughy is concatenating two deep trees, a recursive operation
# still needs testing & further work
app3(xs, ts, ys) %::% Deep : list : Deep : FingerTree
app3(xs, ts, ys) %as% {
  Deep(xs$prefix,
       app3(xs$middle, 
            nodes(c(xs$suffix, ts, ys$prefix)), 
            ys$middle),
       ys$suffix)
}

concat(xs, ys) %::% FingerTree : FingerTree : FingerTree
concat(xs, ys) %as% {
  app3(xs, list(), ys)
}
