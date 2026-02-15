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

# Runtime: O(n) worst-case in relevant input/subtree size.
concat(xs, ys, monoids) %::% FingerTree : FingerTree : list : FingerTree
concat(xs, ys, monoids) %as% {
  .app3_fast(xs, list(), ys, monoids)
}
