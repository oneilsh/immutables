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
xyzs <- tree_from(as.list(letters[16:26]))

plot_tree(abcs)
warnings()

plot_tree(xyzs)

all <- concat_trees(abcs, xyzs)
plot_tree(sample(letters, size = 37, replace = TRUE) |> 
            as.list() |> tree_from(),
          vertex.size = 9, title = "all!", node_label = "both")


indices <- sample(1:26)
mix26 <- tree_from(letters, indices)
plot_tree(mix26, vertex.size = 9, title = "valueed")


catter <- Monoid(function(a, b) paste0(a, b), "")
print(reduce_right(mix26, catter))


valueMinner <- Monoid(function(a, b) {
  if(attr(a, "value") < attr(b, "value")) {a} else {b}
}, structure(Inf, value = Inf))

test <- reduce_left(mix26, valueMinner)
print(test)


valueSummer <- Monoid(function(a, b) {
  structure(
    paste0(a, b),
    value = attr(a, "value") + attr(b, "value")
  )
}, structure("", value = 0))

test <- tree_from(1:10)
um <- reduce_left(test, valueSummer)
#str(test)
um2 <- reduce_left(test, valueSummer)


collector <- Monoid(c, list())
consonants <- Monoid(function(a, b) {
                        vowels <- c("a", "e", "i", "o", "u")
                        c(a[!a %in% vowels], b[!b %in% vowels])
                      },
                      list())

#plot_tree(mix26)
print(reduce_left(mix26, consonants) %>% unlist())


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
