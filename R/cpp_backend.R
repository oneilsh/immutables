# Runtime: O(1).
.ft_cpp_enabled <- function() {
  isTRUE(getOption("immutables.use_cpp", TRUE))
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
  .Call("ft_cpp_append_right", t, el, monoids, PACKAGE = "immutables")
}

# Runtime: O(log n) near right edge.
.ft_cpp_add_right_named <- function(t, el, name, monoids) {
  .Call("ft_cpp_append_right_named", t, el, name, monoids, PACKAGE = "immutables")
}

# Runtime: O(log n) near left edge.
.ft_cpp_add_left <- function(t, el, monoids) {
  .Call("ft_cpp_prepend_left", t, el, monoids, PACKAGE = "immutables")
}

# Runtime: O(log n) near left edge.
.ft_cpp_add_left_named <- function(t, el, name, monoids) {
  .Call("ft_cpp_prepend_left_named", t, el, name, monoids, PACKAGE = "immutables")
}

# Runtime: O(n log n), n = number of elements.
.ft_cpp_tree_from <- function(elements, monoids) {
  .Call("ft_cpp_tree_from", elements, monoids, PACKAGE = "immutables")
}

# Runtime: O(n log n), n = number of elements.
.ft_cpp_tree_from_prepared <- function(elements, values, names, monoids) {
  .Call("ft_cpp_tree_from_prepared", elements, values, names, monoids, PACKAGE = "immutables")
}

# Runtime: O(log n + log m) typical spine depth for balanced trees.
.ft_cpp_concat <- function(x, y, monoids) {
  .Call("ft_cpp_concat", x, y, monoids, PACKAGE = "immutables")
}

# Runtime: O(log n) near locate point depth.
.ft_cpp_locate <- function(t, predicate, monoids, monoid_name, accumulator) {
  .Call("ft_cpp_locate", t, predicate, monoids, monoid_name, accumulator, PACKAGE = "immutables")
}

# Runtime: O(log n) near split point depth.
.ft_cpp_split_tree <- function(t, predicate, monoids, monoid_name, accumulator) {
  .Call("ft_cpp_split_tree", t, predicate, monoids, monoid_name, accumulator, PACKAGE = "immutables")
}

# Runtime: O(n) worst-case, O(k) until first matched name.
.ft_cpp_find_name_position <- function(t, name) {
  .Call("ft_cpp_find_name_position", t, name, PACKAGE = "immutables")
}

# Runtime: O(log n) near queried index depth.
.ft_cpp_get_by_index <- function(t, idx) {
  .Call("ft_cpp_get_by_index", t, as.integer(idx), PACKAGE = "immutables")
}

# Runtime: O(m log n), where m is length(idx_vec).
.ft_cpp_get_many_by_index <- function(t, idx_vec) {
  .Call("ft_cpp_get_many_by_index", t, as.integer(idx_vec), PACKAGE = "immutables")
}

# Runtime: O(n) in tree size.
.ft_cpp_name_positions <- function(t) {
  .Call("ft_cpp_name_positions", t, PACKAGE = "immutables")
}
