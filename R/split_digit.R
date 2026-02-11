# split a digit at the first point where predicate flips from FALSE to TRUE
split_digit(p, i, digit, monoids, monoid_name) %::% Function : . : list : list : character : list
split_digit(p, i, digit, monoids, monoid_name) %as% {
  if(length(digit) == 0) {
    stop("split_digit called with empty digit")
  }

  ms <- ensure_size_monoids(monoids)
  mr <- ms[[monoid_name]]
  if(is.null(mr)) {
    stop(paste0("Unknown measure monoid '", monoid_name, "'."))
  }

  acc <- i
  for(idx in seq_along(digit)) {
    el <- digit[[idx]]
    # el can be a raw element or a structural node.
    m_el <- if(is_structural_node(el)) {
      cached <- attr(el, "measures", exact = TRUE)
      if(!is.null(cached) && !is.null(cached[[monoid_name]])) {
        cached[[monoid_name]]
      } else {
        # support unannotated Node2/Node3 values in tests and ad-hoc usage.
        measure_child_named_impl(el, ms, monoid_name, mr)
      }
    } else {
      mr$measure(el)
    }
    acc <- mr$f(acc, m_el)
    if(p(acc)) {
      left <- if(idx == 1) list() else digit[1:(idx - 1)]
      right <- if(idx == length(digit)) list() else digit[(idx + 1):length(digit)]
      return(list(left = left, elem = el, right = right))
    }
  }

  stop("split_digit: predicate never became true; precondition violated")
}
