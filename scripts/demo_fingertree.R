# This script explores 2-3 finger trees.
library(igraph)
## using base anonymous functions (avoid lambdass/igraph conflicts)
library(magrittr)
library(pryr)
library(rstackdeque)

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  library(fingertree)
}

##############################
## Splitting and concatenating - work in progress
##############################

# baseline monoid for building trees; keeps a cheap size measure
#size_monoid <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)

# ## just for figure development
# t1 <- empty_tree()
# for(i in sample(toupper(c(letters[1:12], "L")))) {
#   t1 <- prepend(t1, i)
# }
#
# # manually exchange a 3-node with a 2-node for illustration;
# # normally 2-nodes are only created during merge operations
# t1$middle$prefix[[1]] <- Node2("B", "F")
#
# plot_tree(t1, vertex.size = 8, edge.width = 1.5)


abcs <- tree_from(as.list(letters[1:12]))
xyzs <- tree_from(as.list(letters[16:26]), monoid = size_monoid)

plot_tree(abcs)
warnings()

plot_tree(xyzs)

all <- concat_trees(abcs, xyzs)
rand_tree <- tree_from(as.list(sample(letters, size = 37, replace = TRUE)), monoid = size_monoid)
plot_tree(rand_tree,
          vertex.size = 9, title = "all!", node_label = "both")


indices <- sample(1:26)
mix26 <- tree_from(letters, indices, monoid = size_monoid)
plot_tree(mix26, vertex.size = 9, title = "valueed")


catter <- MeasureMonoid(function(a, b) paste0(a, b), "", function(el) "")
print(reduce_right(mix26, catter))


valueMinner <- MeasureMonoid(function(a, b) {
  if(attr(a, "value") < attr(b, "value")) {a} else {b}
}, structure(Inf, value = Inf), function(el) attr(el, "value"))

test <- reduce_left(mix26, valueMinner)
print(test)


valueSummer <- MeasureMonoid(function(a, b) {
  structure(
    paste0(a, b),
    value = attr(a, "value") + attr(b, "value")
  )
}, structure("", value = 0), function(el) attr(el, "value"))

test <- tree_from(1:10, monoid = size_monoid)
um <- reduce_left(test, valueSummer)
#str(test)
um2 <- reduce_left(test, valueSummer)


collector <- MeasureMonoid(c, list(), function(el) list())
consonants <- MeasureMonoid(function(a, b) {
                        vowels <- c("a", "e", "i", "o", "u")
                        c(a[!a %in% vowels], b[!b %in% vowels])
                      },
                      list(),
                      function(el) list())

#plot_tree(mix26)
print(reduce_left(mix26, consonants) %>% unlist())

# indexing examples (read + replacement)
idx_tree <- tree_from(1:8, monoid = size_monoid)
print(idx_tree[[4]])           # 4
print(reduce_left(idx_tree[c(2, 4, 6)]))

idx_tree2 <- idx_tree
idx_tree2[[3]] <- 99
idx_tree2[c(1, 8)] <- list(111, 888)
print(reduce_left(idx_tree2))


#### this doesn't work...

# fs <- tree_from(list(
#   abs,
#   log,
#   sqrt
# ))
#
# applyer <- Monoid(function(f1, f2) {
#   function(...) {
#       f1(f2(...))
#     }
#   },
# I)
# f_all <- reduce_left(fs, applyer)
#
# x <- c(3, -5, 2, -4, 1)
# abs(log(sqrt(x)))
# sqrt(log(abs(x)))
# f_all(x)
