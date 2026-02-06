# This script explores 2-3 finger trees, using lambda.r
# (exploring typed structures similar to examples of 2-3 finger in haskell)
library(lambda.r)
library(igraph)
library(lambdass)  # shorter/nicer anonymous function syntax
library(magrittr)
library(rlist)
library(rstackdeque)
library(pryr)
library(memoise)

source(file.path("R", "constructors.R"))
source(file.path("R", "reduce_left.R"))
source(file.path("R", "reduce_right.R"))
source(file.path("R", "add_left.R"))
source(file.path("R", "add_right.R"))
source(file.path("R", "concat.R"))
source(file.path("R", "split.R"))
source(file.path("R", "utils.R"))

##############################
## Splitting and concatenating - work in progress
##############################

# ## just for figure development
# t1 <- Empty()
# for(i in sample(toupper(c(letters[1:12], "L")))) {
#   t1 <- add_left(t1, Element(i))
# }
# 
# # manually exchange a 3-node with a 2-node for illustration; 
# # normally 2-nodes are only created during merge operations
# t1$middle$prefix[[1]] <- Node2(Element("B"), Element("F"))
# 
# plot_tree(t1, vertex.size = 8, edge.width = 1.5)



abcs <- as.FingerTree(as.list(letters[1:12]))
xyzs <- as.FingerTree(as.list(letters[16:26]))

#plot_tree(abcs)
#plot_tree(xyzs)

all <- concat(abcs, xyzs)
plot_tree(all, vertex.size = 9, title = "all!")



indices <- sample(1:26)
mix26 <- as.FingerTree(letters, indices)
plot_tree(mix26, vertex.size = 9, title = "valueed")


catter <- Reducer({a; b} %->% {
  Element(paste0(a, b))
  }, Element(""))
print(reduce_right(mix26, catter))



valueMinner <- Reducer({a; b} %->% {
  if(attr(a, "value") < attr(b, "value")) {a} else {b}
}, Element(Inf))
test <- reduce_left(mix26, valueMinner)
print(test)



valueSummer <- Reducer({a; b} %->% {
  Element(paste0(a, b), value = attr(a, "value") + attr(b, "value"))
}, Element("", value = 0) )
test <- as.FingerTree(1:10)
um <- reduce_left(test, valueSummer)
#str(test)
um2 <- reduce_left(test, valueSummer)


# minner <- Reducer({a; b} %->% {
#   cat("\n")
#   str(b)
#   if(a > b) {a} else {b}
# }, Inf)
# test <- reduce_left(mix26, minner)
# cat("-----")
# str(test)



# 
# collector <- Reducer(c, Element(list()))
# consonants <- Reducer(function(a, b) {
#                         vowels <- c("a", "e", "i", "o", "u")
#                         c(a[!a %in% vowels], b[!b %in% vowels])
#                       }, 
#                       Element(c()))
# 
# #plot_tree(mix26)
# print(reduce_left(mix26, consonants) %>% unlist()) 


#### this doesn't work... 

# fs <- as.FingerTree(list(
#   Element(abs),
#   Element(log),
#   Element(sqrt)
# ))
# 
# applyer <- Reducer(function(f1, f2) {
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
