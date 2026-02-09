# split a digit at the first point where predicate flips from FALSE to TRUE
split_digit <- function(p, i, digit, mr) {
  if(length(digit) == 0) {
    stop("split_digit called with empty digit")
  }

  acc <- i
  for(idx in seq_along(digit)) {
    el <- digit[[idx]]
    # el can be a raw element or a Node; measure_child handles both.
    acc <- mr$f(acc, measure_child(el, mr))
    if(p(acc)) {
      left <- if(idx == 1) list() else digit[1:(idx - 1)]
      right <- if(idx == length(digit)) list() else digit[(idx + 1):length(digit)]
      return(list(left = left, elem = el, right = right))
    }
  }

  stop("split_digit: predicate never became true; precondition violated")
}
