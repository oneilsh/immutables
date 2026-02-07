# check for MeasureReducer type
is_measure_reducer <- function(r) {
  inherits(r, "MeasureReducer")
}

# compute measure for a child node or raw element, using cached values when present
measure_child <- function(x, r) {
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

# combine a list of measure values with the reducer's associative function
combine_measures <- function(measures, r) {
  acc <- r$i
  for(m in measures) {
    acc <- r$f(acc, m)
  }
  acc
}

# attach a measure attribute to a structural node
set_measure <- function(x, r) {
  if(!is_measure_reducer(r)) {
    return(x)
  }
  if(!is_structural_node(x)) {
    return(x)
  }
  attr(x, "measure") <- measure_child(x, r)
  x
}

# construct an Empty with cached measure
measured_empty <- function(r) {
  e <- Empty()
  attr(e, "measure") <- r$i
  e
}

# construct a Single with cached measure
measured_single <- function(el, r) {
  s <- Single(el)
  attr(s, "measure") <- measure_child(el, r)
  s
}

# construct a Digit with cached measure
measured_digit <- function(..., r) {
  d <- Digit(...)
  attr(d, "measure") <- measure_child(d, r)
  d
}

# construct a Node2 with cached measure
measured_node2 <- function(x, y, r) {
  n <- Node2(x, y)
  attr(n, "measure") <- measure_child(n, r)
  n
}

# construct a Node3 with cached measure
measured_node3 <- function(x, y, z, r) {
  n <- Node3(x, y, z)
  attr(n, "measure") <- measure_child(n, r)
  n
}

# construct a Deep with cached measure (uses child measures)
measured_deep <- function(prefix, middle, suffix, r) {
  t <- Deep(prefix, middle, suffix)
  attr(t, "measure") <- measure_child(t, r)
  t
}
