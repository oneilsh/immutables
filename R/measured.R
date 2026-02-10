# check for MeasureMonoid type
is_measure_monoid(r) %::% . : logical
is_measure_monoid(r) %as% inherits(r, "MeasureMonoid")

# a list (named or unnamed) of MeasureMonoid specs
is_measure_monoid_list(x) %::% . : logical
is_measure_monoid_list(x) %as% {
  is.list(x) && length(x) > 0 && all(vapply(x, is_measure_monoid, logical(1)))
}

# default size measure used for indexing semantics
size_measure_monoid() %::% MeasureMonoid
size_measure_monoid() %as% {
  MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
}

ensure_size_monoid(monoids) %::% list : list
ensure_size_monoid(monoids) %as% {
  out <- monoids
  if(is.null(names(out)) || any(names(out) == "")) {
    names(out) <- paste0("m", seq_along(out))
  }
  if(is.null(out[[".size"]])) {
    out <- c(out, list(.size = size_measure_monoid()))
  }
  out
}

# normalize a single MeasureMonoid OR a list of MeasureMonoids into a context
# object that still dispatches as MeasureMonoid (primary monoid in f/i/measure)
as_measure_context(r) %::% . : MeasureMonoid
as_measure_context(r) %as% {
  if(inherits(r, "MeasureMonoidSet")) {
    return(r)
  }

  if(is_measure_monoid(r)) {
    monoids <- ensure_size_monoid(list(.reduce = r))
    ctx <- list(f = r$f, i = r$i, measure = r$measure, monoids = monoids, primary = ".reduce")
    class(ctx) <- c("MeasureMonoidSet", "MeasureMonoid", "list")
    return(ctx)
  }

  if(is_measure_monoid_list(r)) {
    monoids <- ensure_size_monoid(r)
    primary_candidates <- setdiff(names(monoids), ".size")
    primary <- if(length(primary_candidates) > 0) primary_candidates[[1]] else ".size"
    base <- monoids[[1]]
    ctx <- list(f = base$f, i = base$i, measure = base$measure, monoids = monoids, primary = primary)
    class(ctx) <- c("MeasureMonoidSet", "MeasureMonoid", "list")
    return(ctx)
  }

  stop("Expected a MeasureMonoid or a non-empty list of MeasureMonoid objects.")
}

# all monoids from a context
context_monoids(ctx) %::% MeasureMonoid : list
context_monoids(ctx) %as% {
  if(inherits(ctx, "MeasureMonoidSet")) {
    return(ctx$monoids)
  }
  ensure_size_monoid(list(.reduce = ctx))
}

# name of primary monoid in the context
context_primary_name(ctx) %::% MeasureMonoid : character
context_primary_name(ctx) %as% {
  if(inherits(ctx, "MeasureMonoidSet")) {
    return(ctx$primary)
  }
  ".reduce"
}

# combine a list of measure values with the monoid's associative function
combine_measures(measures, r) %::% list : MeasureMonoid : .
combine_measures(measures, r) %as% {
  acc <- r$i
  for(m in measures) {
    acc <- r$f(acc, m)
  }
  acc
}

# compute measure for a child with respect to a single named monoid in a context
measure_child_named(x, ctx, name) %::% . : MeasureMonoid : character : .
measure_child_named(x, ctx, name) %as% {
  monoids <- context_monoids(ctx)
  r <- monoids[[name]]
  if(is.null(r)) {
    stop(paste0("Unknown measure monoid '", name, "'."))
  }

  if(is_structural_node(x)) {
    cached_all <- attr(x, "measures")
    if(!is.null(cached_all) && !is.null(cached_all[[name]])) {
      return(cached_all[[name]])
    }

    primary <- context_primary_name(ctx)
    if(identical(name, primary)) {
      cached_primary <- attr(x, "measure")
      if(!is.null(cached_primary)) {
        return(cached_primary)
      }
    }

    if(x %isa% Empty) {
      return(r$i)
    }
    if(x %isa% Single) {
      return(measure_child_named(.subset2(x, 1), ctx, name))
    }
    if(x %isa% Deep) {
      return(combine_measures(
        list(
          measure_child_named(x$prefix, ctx, name),
          measure_child_named(x$middle, ctx, name),
          measure_child_named(x$suffix, ctx, name)
        ),
        r
      ))
    }

    return(combine_measures(lapply(x, measure_child_named, ctx = ctx, name = name), r))
  }

  r$measure(x)
}

# compute measure for a child node or raw element, using cached values when present
measure_child(x, r) %::% . : MeasureMonoid : .
measure_child(x, r) %as% {
  ctx <- as_measure_context(r)
  measure_child_named(x, ctx, context_primary_name(ctx))
}

# compute all cached measures for a child across all monoids in context
measure_children(x, r) %::% . : MeasureMonoid : list
measure_children(x, r) %as% {
  ctx <- as_measure_context(r)
  monoids <- context_monoids(ctx)
  out <- lapply(names(monoids), function(nm) measure_child_named(x, ctx, nm))
  names(out) <- names(monoids)
  out
}

# attach cached measure attributes to a structural node:
# - "measures": named list of all measure values
# - "measure": primary measure value for backwards compatibility
set_measure(x, r) %::% . : MeasureMonoid : .
set_measure(x, r) %as% {
  if(!is_structural_node(x)) {
    return(x)
  }
  ctx <- as_measure_context(r)
  all_measures <- measure_children(x, ctx)
  attr(x, "measures") <- all_measures
  attr(x, "measure") <- all_measures[[context_primary_name(ctx)]]
  x
}

# construct an Empty with cached measure
measured_empty(r) %::% MeasureMonoid : Empty
measured_empty(r) %as% {
  e <- Empty()
  set_measure(e, r)
}

# construct a Single with cached measure
measured_single(el, r) %::% . : MeasureMonoid : Single
measured_single(el, r) %as% {
  s <- Single(el)
  set_measure(s, r)
}

# construct a Digit with cached measure (size 1..4)
measured_digit(a, r) %::% . : MeasureMonoid : Digit
measured_digit(a, r) %as% {
  d <- Digit(a)
  set_measure(d, r)
}

measured_digit(a, b, r) %::% . : . : MeasureMonoid : Digit
measured_digit(a, b, r) %as% {
  d <- Digit(a, b)
  set_measure(d, r)
}

measured_digit(a, b, c, r) %::% . : . : . : MeasureMonoid : Digit
measured_digit(a, b, c, r) %as% {
  d <- Digit(a, b, c)
  set_measure(d, r)
}

measured_digit(a, b, c, d1, r) %::% . : . : . : . : MeasureMonoid : Digit
measured_digit(a, b, c, d1, r) %as% {
  d <- Digit(a, b, c, d1)
  set_measure(d, r)
}

# construct a Node2 with cached measure
measured_node2(x, y, r) %::% . : . : MeasureMonoid : Node
measured_node2(x, y, r) %as% {
  n <- Node2(x, y)
  set_measure(n, r)
}

# construct a Node3 with cached measure
measured_node3(x, y, z, r) %::% . : . : . : MeasureMonoid : Node
measured_node3(x, y, z, r) %as% {
  n <- Node3(x, y, z)
  set_measure(n, r)
}

# construct a Deep with cached measure (uses child measures)
measured_deep(prefix, middle, suffix, r) %::% Digit : FingerTree : Digit : MeasureMonoid : Deep
measured_deep(prefix, middle, suffix, r) %as% {
  t <- Deep(prefix, middle, suffix)
  set_measure(t, r)
}
