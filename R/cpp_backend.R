# Runtime: O(1).
.ft_cpp_enabled <- function() {
  isTRUE(getOption("fingertree.use_cpp", TRUE))
}

# Runtime: O(m), where m is monoid count.
.ft_cpp_eligible_monoids <- function(monoids) {
  if(is.null(monoids) || !is.list(monoids) || is.null(names(monoids))) {
    return(FALSE)
  }
  TRUE
}

# Runtime: O(1).
.ft_cpp_can_use <- function(monoids) {
  .ft_cpp_enabled() && .ft_cpp_eligible_monoids(monoids)
}

# Runtime: O(log n) near right edge.
.ft_cpp_add_right <- function(t, el, monoids) {
  .Call("ft_cpp_append_right", t, el, monoids, PACKAGE = "fingertree")
}

# Runtime: O(log n) near right edge.
.ft_cpp_add_right_named <- function(t, el, name, monoids) {
  .Call("ft_cpp_append_right_named", t, el, name, monoids, PACKAGE = "fingertree")
}

# Runtime: O(log n) near left edge.
.ft_cpp_add_left <- function(t, el, monoids) {
  .Call("ft_cpp_prepend_left", t, el, monoids, PACKAGE = "fingertree")
}

# Runtime: O(log n) near left edge.
.ft_cpp_add_left_named <- function(t, el, name, monoids) {
  .Call("ft_cpp_prepend_left_named", t, el, name, monoids, PACKAGE = "fingertree")
}

# Runtime: O(n log n), n = number of elements.
.ft_cpp_tree_from <- function(elements, monoids) {
  .Call("ft_cpp_tree_from", elements, monoids, PACKAGE = "fingertree")
}

# Runtime: O(n log n), n = number of elements.
.ft_cpp_tree_from_prepared <- function(elements, values, names, monoids) {
  .Call("ft_cpp_tree_from_prepared", elements, values, names, monoids, PACKAGE = "fingertree")
}

# Runtime: O(log n + log m) typical spine depth for balanced trees.
.ft_cpp_concat <- function(x, y, monoids) {
  .Call("ft_cpp_concat", x, y, monoids, PACKAGE = "fingertree")
}
