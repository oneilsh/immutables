# Extra playground script for current fingertree API.
library(igraph)
library(magrittr)
library(pryr)
library(rstackdeque)

if(requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  library(fingertree)
}

set.seed(7)
size_monoids <- list(.size = size_measure_monoid())

left <- tree_from(as.list(letters[1:12]), monoids = size_monoids)
right <- tree_from(as.list(letters[16:26]), monoids = size_monoids)

# Shared monoid names warn during concat by design.
both <- suppressWarnings(concat_trees(left, right))
plot_tree(both, vertex.size = 9, title = "concat")

valued <- tree_from(letters, values = sample(1:26), monoids = size_monoids)
plot_tree(valued, vertex.size = 9, title = "valued")

char_concat <- MeasureMonoid(
  function(a, b) paste0(a, b),
  "",
  function(el) as.character(el)
)
print(reduce_right(valued, char_concat))

value_min <- MeasureMonoid(
  min,
  Inf,
  function(el) as.numeric(attr(el, "value"))
)
value_sum <- MeasureMonoid(
  `+`,
  0,
  function(el) as.numeric(attr(el, "value"))
)
print(reduce_left(valued, value_min))
print(reduce_left(valued, value_sum))

# Split by size monoid name.
s <- split(both, function(v) v >= 7, ".size")
print(reduce_left(s$left, size_measure_monoid()))
print(reduce_left(s$right, size_measure_monoid()))

# Add a custom monoid to an existing tree.
numeric_tree <- tree_from(1:10, monoids = size_monoids)
sum_numeric <- MeasureMonoid(`+`, 0, as.numeric)
numeric_tree2 <- add_monoids(numeric_tree, list(sum = sum_numeric))
print(attr(numeric_tree2, "measures"))
