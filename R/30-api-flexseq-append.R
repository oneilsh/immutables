#' Append elements
#'
#' Generic function for appending. The default method calls [base::append()].
#' The flexseq method appends a single element to the right side of a
#' persistent sequence.
#'
#' @param x Object to append to.
#' @param ... Method-specific arguments.
#' @return Updated object.
#' @export
#' @noRd
append <- function(x, ...) {
  UseMethod("append")
}

#' @export
#' @noRd
append.default <- function(x, values, after = length(x), ...) {
  base::append(x, values, after)
}

#' Append an element to a flexseq
#'
#' @name append.flexseq
#' @aliases append
#' @method append flexseq
#' @param x A `flexseq`.
#' @param values Element to append.
#' @param ... Unused.
#' @return Updated `flexseq`.
#' @examples
#' x <- as_flexseq(letters[1:4])
#' x
#'
#' x2 <- append(x, "p")
#' x2
#'
#' # flexseqs can also hold nested or mixed types
#' x3 <- append(x2, c(8, 9))
#' x3
#'
#' # appending to a named sequence requires a named element
#' x4 <- as_flexseq(list(one = 1, two = 2, three = 3))
#' new_el <- 4
#' names(new_el) <- "four"
#' x5 <- append(x4, new_el)
#' x5
#' @export
# Runtime: O(log n) tree update, with O(1) local name-state checks.
append.flexseq <- function(x, values, ...) {
  ms <- attr(x, "monoids", exact = TRUE)
  if(is.null(ms)) {
    stop("Tree has no monoids attribute.")
  }
  m <- attr(x, "measures", exact = TRUE)
  if(is.null(m)) {
    stop("Tree has no measures attribute.")
  }
  n <- as.integer(m[[".size"]])
  nn <- as.integer(m[[".named_count"]])
  # Runtime: O(1). Enforce non-mixed naming invariant without full traversal.
  if(n > 0L && nn != 0L && nn != n) {
    stop("Invalid tree name state: mixed named/unnamed elements.")
  }

  x2 <- values
  if(nn == 0L) {
    # Fast unnamed path: avoid attr writes when incoming element is also unnamed.
    nm <- .ft_get_name(values)
    if(is.null(nm)) {
      nm <- .ft_name_from_value(values)
    }
    if(is.null(nm)) {
      if(.ft_cpp_can_use(ms)) {
        return(.as_flexseq(.ft_cpp_add_right(x, x2, ms)))
      }
      return(.as_flexseq(.add_right_fast(x, x2, ms)))
    }
    if(n > 0L) {
      stop("Cannot mix named and unnamed elements (append would create mixed named and unnamed tree).")
    }
    if(.ft_cpp_can_use(ms)) {
      return(.as_flexseq(.ft_cpp_add_right_named(x, values, nm, ms)))
    }
    x2 <- .ft_set_name(x2, nm)
  } else {
    nm <- .ft_effective_name(values)
    if(is.null(nm)) {
      stop("Cannot mix named and unnamed elements (append would create mixed named and unnamed tree).")
    }
    if(.ft_cpp_can_use(ms)) {
      return(.as_flexseq(.ft_cpp_add_right_named(x, values, nm, ms)))
    }
    x2 <- .ft_set_name(x2, nm)
  }

  if(.ft_cpp_can_use(ms)) {
    return(.as_flexseq(.ft_cpp_add_right(x, x2, ms)))
  }
  .as_flexseq(.add_right_fast(x, x2, ms))
}
