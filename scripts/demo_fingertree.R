# Demo script for the current fingertree API.
library(igraph)
library(magrittr)
library(pryr)
library(rstackdeque)

if(requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  library(fingertree)
}

set.seed(42)
size_monoids <- list(.size = size_measure_monoid())

abcs <- tree_from(as.list(letters[1:12]), monoids = size_monoids)
xyzs <- tree_from(as.list(letters[16:26]), monoids = size_monoids)

plot_tree(abcs, title = "abcs")
plot_tree(xyzs, title = "xyzs")

# This warns by design when same monoid names exist on both sides.
combined <- suppressWarnings(concat_trees(abcs, xyzs))
plot_tree(combined, vertex.size = 9, title = "combined", node_label = "both")

mix26 <- tree_from(letters, values = sample(1:26), monoids = size_monoids)
plot_tree(mix26, vertex.size = 9, title = "valued letters")

# Reduce examples: explicit MeasureMonoid is always required.
char_concat <- MeasureMonoid(
  function(a, b) paste0(a, b),
  "",
  function(el) as.character(el)
)
print(reduce_right(mix26, char_concat))

value_min <- MeasureMonoid(
  min,
  Inf,
  function(el) as.numeric(attr(el, "value"))
)
print(reduce_left(mix26, value_min))

value_sum <- MeasureMonoid(
  `+`,
  0,
  function(el) as.numeric(attr(el, "value"))
)
print(reduce_left(mix26, value_sum))

vowels <- c("a", "e", "i", "o", "u")
consonants <- MeasureMonoid(
  c,
  character(),
  function(el) if(el %in% vowels) character() else as.character(el)
)
print(reduce_left(mix26, consonants))

# Indexing examples.
idx_tree <- tree_from(1:8, monoids = size_monoids)
print(idx_tree[[4]]) # 4
sum_numeric <- MeasureMonoid(`+`, 0, as.numeric)
print(reduce_left(idx_tree[c(2, 4, 6)], sum_numeric))

idx_tree2 <- idx_tree
idx_tree2[[3]] <- 99
idx_tree2[c(1, 8)] <- list(111, 888)
print(reduce_left(idx_tree2, size_measure_monoid()))

# Split example (.size split at first prefix with size >= 5).
s <- split(idx_tree2, function(v) v >= 5, ".size")
print(reduce_left(s$left, size_measure_monoid()))
print(reduce_left(s$right, size_measure_monoid()))

# Add monoid post-hoc and inspect cached root measures.
idx_tree3 <- add_monoids(idx_tree2, list(sum = sum_numeric))
print(attr(idx_tree3, "measures"))
