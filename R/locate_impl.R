# read-only locate helpers: no reconstruction, only traversal + metadata.

locate_child_measure(x, ms, monoid_name, mr) %::% . : list : character : MeasureMonoid : .
locate_child_measure(x, ms, monoid_name, mr) %as% {
  if(is_structural_node(x)) {
    cached <- attr(x, "measures", exact = TRUE)
    if(!is.null(cached) && !is.null(cached[[monoid_name]])) {
      return(cached[[monoid_name]])
    }
    return(measure_child_named_impl(x, ms, monoid_name, mr))
  }
  mr$measure(x)
}

locate_child_size(x, ms) %::% . : list : integer
locate_child_size(x, ms) %as% {
  sr <- ms[[".size"]]
  if(is.null(sr)) {
    return(1L)
  }
  out <- if(is_structural_node(x)) {
    as.integer(node_measure(x, ".size"))
  } else {
    as.integer(sr$measure(x))
  }
  if(is.na(out) || out < 0L) {
    stop(".size monoid must produce non-negative integer sizes.")
  }
  out
}

locate_sequence_measure(xs, ms, monoid_name, mr) %::% list : list : character : MeasureMonoid : .
locate_sequence_measure(xs, ms, monoid_name, mr) %as% {
  if(length(xs) == 0L) {
    return(mr$i)
  }
  acc <- mr$i
  for(el in xs) {
    acc <- mr$f(acc, locate_child_measure(el, ms, monoid_name, mr))
  }
  acc
}

locate_digit(p, i, digit, monoids, monoid_name, i_size) %::% Function : . : list : list : character : integer : list
locate_digit(p, i, digit, monoids, monoid_name, i_size = 0L) %as% {
  if(length(digit) == 0) {
    stop("locate_digit called with empty digit")
  }

  ms <- ensure_size_monoids(monoids)
  mr <- ms[[monoid_name]]
  if(is.null(mr)) {
    stop(paste0("Unknown measure monoid '", monoid_name, "'."))
  }

  acc <- i
  size_before <- as.integer(i_size)

  for(idx in seq_along(digit)) {
    el <- digit[[idx]]
    m_el <- locate_child_measure(el, ms, monoid_name, mr)
    n_el <- locate_child_size(el, ms)
    acc_after <- mr$f(acc, m_el)

    if(p(acc_after)) {
      if(is_structural_node(el)) {
        return(locate_tree_impl(p, acc, el, ms, monoid_name, size_before))
      }

      right <- if(idx == length(digit)) list() else digit[(idx + 1):length(digit)]
      right_measure <- locate_sequence_measure(right, ms, monoid_name, mr)
      return(list(
        found = TRUE,
        elem = el,
        left_measure = acc,
        hit_measure = acc_after,
        right_measure = right_measure,
        index = size_before + 1L
      ))
    }

    acc <- acc_after
    size_before <- size_before + n_el
  }

  list(found = FALSE, elem = NULL, left_measure = acc, hit_measure = NULL, right_measure = NULL, index = NULL)
}

locate_tree_impl(p, i, t, monoids, monoid_name, i_size) %::% Function : . : . : list : character : integer : list
locate_tree_impl(p, i, t, monoids, monoid_name, i_size = 0L) %as% {
  ms <- ensure_size_monoids(monoids)
  mr <- ms[[monoid_name]]
  if(is.null(mr)) {
    stop(paste0("Unknown measure monoid '", monoid_name, "'."))
  }

  if(!is_structural_node(t)) {
    return(locate_digit(p, i, list(t), ms, monoid_name, as.integer(i_size)))
  }

  if(t %isa% Empty) {
    return(list(found = FALSE, elem = NULL, left_measure = i, hit_measure = NULL, right_measure = NULL, index = NULL))
  }

  if(t %isa% Single) {
    return(locate_digit(p, i, list(.subset2(t, 1)), ms, monoid_name, as.integer(i_size)))
  }

  if(t %isa% Deep) {
    mpr <- node_measure(.subset2(t,"prefix"), monoid_name)
    mm <- node_measure(.subset2(t,"middle"), monoid_name)
    msf <- node_measure(.subset2(t,"suffix"), monoid_name)

    vpr <- mr$f(i, mpr)
    vm <- mr$f(vpr, mm)

    npr <- as.integer(node_measure(.subset2(t,"prefix"), ".size"))
    nm <- as.integer(node_measure(.subset2(t,"middle"), ".size"))

    if(p(vpr)) {
      res <- locate_tree_impl(p, i, .subset2(t,"prefix"), ms, monoid_name, as.integer(i_size))
      if(res$found) {
        res$right_measure <- mr$f(mr$f(res$right_measure, mm), msf)
      }
      return(res)
    }

    if(p(vm)) {
      res <- locate_tree_impl(p, vpr, .subset2(t,"middle"), ms, monoid_name, as.integer(i_size + npr))
      if(res$found) {
        res$right_measure <- mr$f(res$right_measure, msf)
      }
      return(res)
    }

    return(locate_tree_impl(p, vm, .subset2(t,"suffix"), ms, monoid_name, as.integer(i_size + npr + nm)))
  }

  # Node / Digit structural lists
  locate_digit(p, i, as.list(t), ms, monoid_name, as.integer(i_size))
}
