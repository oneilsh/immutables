# check for MeasureMonoid type
is_measure_monoid(r) %::% . : logical
is_measure_monoid(r) %as% inherits(r, "MeasureMonoid")

# combine a list of measure values with the monoid's associative function
combine_measures(measures, r) %::% list : MeasureMonoid : .
combine_measures(measures, r) %as% {
  acc <- r$i
  for(m in measures) {
    acc <- r$f(acc, m)
  }
  acc
}

# compute measure for a child node or raw element, using cached values when present
measure_child(x, r) %::% . : MeasureMonoid : .
measure_child(x, r) %as% {
  if(is_structural_node(x)) {
    cached <- attr(x, "measure")
    if(!is.null(cached)) {
      return(cached)
    }

    if(x %isa% Empty) {
      return(r$i)
    }
    if(x %isa% Single) {
      return(measure_child(x[[1]], r))
    }
    if(x %isa% Deep) {
      return(combine_measures(
        list(
          measure_child(x$prefix, r),
          measure_child(x$middle, r),
          measure_child(x$suffix, r)
        ),
        r
      ))
    }

    # Node or Digit
    return(combine_measures(lapply(x, measure_child, r = r), r))
  }

  r$measure(x)
}

# attach a measure attribute to a structural node
set_measure(x, r) %::% . : MeasureMonoid : .
set_measure(x, r) %as% {
  if(!is_structural_node(x)) {
    return(x)
  }
  attr(x, "measure") <- measure_child(x, r)
  x
}

# construct an Empty with cached measure
measured_empty(r) %::% MeasureMonoid : Empty
measured_empty(r) %as% {
  e <- Empty()
  attr(e, "measure") <- r$i
  e
}

# construct a Single with cached measure
measured_single(el, r) %::% . : MeasureMonoid : Single
measured_single(el, r) %as% {
  s <- Single(el)
  attr(s, "measure") <- measure_child(el, r)
  s
}

# construct a Digit with cached measure (size 1..4)
measured_digit(a, r) %::% . : MeasureMonoid : Digit
measured_digit(a, r) %as% {
  d <- Digit(a)
  attr(d, "measure") <- measure_child(d, r)
  d
}

measured_digit(a, b, r) %::% . : . : MeasureMonoid : Digit
measured_digit(a, b, r) %as% {
  d <- Digit(a, b)
  attr(d, "measure") <- measure_child(d, r)
  d
}

measured_digit(a, b, c, r) %::% . : . : . : MeasureMonoid : Digit
measured_digit(a, b, c, r) %as% {
  d <- Digit(a, b, c)
  attr(d, "measure") <- measure_child(d, r)
  d
}

measured_digit(a, b, c, d1, r) %::% . : . : . : . : MeasureMonoid : Digit
measured_digit(a, b, c, d1, r) %as% {
  d <- Digit(a, b, c, d1)
  attr(d, "measure") <- measure_child(d, r)
  d
}

# construct a Node2 with cached measure
measured_node2(x, y, r) %::% . : . : MeasureMonoid : Node
measured_node2(x, y, r) %as% {
  n <- Node2(x, y)
  attr(n, "measure") <- measure_child(n, r)
  n
}

# construct a Node3 with cached measure
measured_node3(x, y, z, r) %::% . : . : . : MeasureMonoid : Node
measured_node3(x, y, z, r) %as% {
  n <- Node3(x, y, z)
  attr(n, "measure") <- measure_child(n, r)
  n
}

# construct a Deep with cached measure (uses child measures)
measured_deep(prefix, middle, suffix, r) %::% Digit : FingerTree : Digit : MeasureMonoid : Deep
measured_deep(prefix, middle, suffix, r) %as% {
  t <- Deep(prefix, middle, suffix)
  attr(t, "measure") <- measure_child(t, r)
  t
}
