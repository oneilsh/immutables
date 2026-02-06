is_measured_reducer <- function(r) {
  inherits(r, "MeasuredReducer")
}

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

combine_measures <- function(measures, r) {
  acc <- r$i
  for(m in measures) {
    acc <- r$f(acc, m)
  }
  acc
}

set_measure <- function(x, r) {
  if(!is_measured_reducer(r)) {
    return(x)
  }
  if(!is_structural_node(x)) {
    return(x)
  }
  attr(x, "measure") <- measure_child(x, r)
  x
}

measured_empty <- function(r) {
  e <- Empty()
  attr(e, "measure") <- r$i
  e
}

measured_single <- function(el, r) {
  s <- Single(el)
  attr(s, "measure") <- measure_child(el, r)
  s
}

measured_digit <- function(..., r) {
  d <- Digit(...)
  attr(d, "measure") <- measure_child(d, r)
  d
}

measured_node2 <- function(x, y, r) {
  n <- Node2(x, y)
  attr(n, "measure") <- measure_child(n, r)
  n
}

measured_node3 <- function(x, y, z, r) {
  n <- Node3(x, y, z)
  attr(n, "measure") <- measure_child(n, r)
  n
}

measured_deep <- function(prefix, middle, suffix, r) {
  t <- Deep(prefix, middle, suffix)
  attr(t, "measure") <- measure_child(t, r)
  t
}
