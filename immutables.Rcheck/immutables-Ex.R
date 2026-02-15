pkgname <- "immutables"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('immutables')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_monoids")
### * add_monoids

flush(stderr()); flush(stdout())

### Name: add_monoids
### Title: Add/merge monoids on an existing tree
### Aliases: add_monoids

### ** Examples

t <- as_flexseq(1:5)
sum_m <- measure_monoid(`+`, 0, as.numeric)
t2 <- add_monoids(t, list(sum = sum_m))
attr(t2, "measures")$sum

# Replace an existing monoid definition
sum2 <- measure_monoid(function(a, b) a + 2 * b, 0, as.numeric)
t3 <- add_monoids(t2, list(sum = sum2), overwrite = TRUE)
attr(t3, "measures")$sum



cleanEx()
nameEx("append.flexseq")
### * append.flexseq

flush(stderr()); flush(stdout())

### Name: append.flexseq
### Title: Append an element to a flexseq
### Aliases: append.flexseq

### ** Examples

t <- as_flexseq(letters[1:3])
t2 <- append(t, "d")
t2[[4]]

# Append to a named sequence
tn <- as_flexseq(setNames(as.list(1:2), c("a", "b")))
tn2 <- append(tn, stats::setNames(3, "c"))
tn2[["c"]]



cleanEx()
nameEx("as_flexseq")
### * as_flexseq

flush(stderr()); flush(stdout())

### Name: as_flexseq
### Title: Coerce to a Persistent Flexible Sequence
### Aliases: as_flexseq

### ** Examples

as_flexseq(1:5)
as_flexseq(setNames(as.list(letters[1:3]), c("k1", "k2", "k3")))



cleanEx()
nameEx("as_priority_queue")
### * as_priority_queue

flush(stderr()); flush(stdout())

### Name: as_priority_queue
### Title: Build a Priority Queue from elements and priorities
### Aliases: as_priority_queue

### ** Examples

q <- as_priority_queue(letters[1:4], priorities = c(3, 1, 2, 1))
peek_min(q)
peek_max(q)



cleanEx()
nameEx("cash-.FingerTree")
### * cash-.FingerTree

flush(stderr()); flush(stdout())

### Name: $.FingerTree
### Title: Extract a named element with '$'
### Aliases: $.FingerTree

### ** Examples

t <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
t$b



cleanEx()
nameEx("cash-set-.FingerTree")
### * cash-set-.FingerTree

flush(stderr()); flush(stdout())

### Name: $<-.FingerTree
### Title: Replace a named element with '$<-'
### Aliases: $<-.FingerTree

### ** Examples

t <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
t$b <- 20
t$b



cleanEx()
nameEx("concat_trees")
### * concat_trees

flush(stderr()); flush(stdout())

### Name: concat_trees
### Title: Concatenate Two Structural Trees
### Aliases: concat_trees
### Keywords: internal

### ** Examples

## Not run: 
##D left <- as_flexseq(letters[1:3])
##D right <- as_flexseq(letters[4:6])
##D t <- concat_trees(left, right)
##D cat_m <- measure_monoid(paste0, "", as.character)
##D fold_left(t, cat_m)
## End(Not run)



cleanEx()
nameEx("empty_tree")
### * empty_tree

flush(stderr()); flush(stdout())

### Name: empty_tree
### Title: Create an Empty Structural Tree
### Aliases: empty_tree
### Keywords: internal

### ** Examples

## Not run: 
##D t <- empty_tree()
##D t
##D 
##D count_m <- measure_monoid(`+`, 0, function(el) 1)
##D t2 <- empty_tree(monoids = list(count = count_m))
##D attr(t2, "measures")
## End(Not run)



cleanEx()
nameEx("extract_max")
### * extract_max

flush(stderr()); flush(stdout())

### Name: extract_max
### Title: Extract maximum-priority element
### Aliases: extract_max

### ** Examples

q <- priority_queue("a", "b", "c", priorities = c(2, 3, 3))
out <- extract_max(q)
out$element
out$priority



cleanEx()
nameEx("extract_min")
### * extract_min

flush(stderr()); flush(stdout())

### Name: extract_min
### Title: Extract minimum-priority element
### Aliases: extract_min

### ** Examples

q <- priority_queue("a", "b", "c", priorities = c(2, 1, 1))
out <- extract_min(q)
out$element
out$priority



cleanEx()
nameEx("flexseq")
### * flexseq

flush(stderr()); flush(stdout())

### Name: flexseq
### Title: Construct a Persistent Flexible Sequence
### Aliases: flexseq

### ** Examples

x <- flexseq(1, 2, 3)
x[[2]]

y <- flexseq(a = "x", b = "y")
y$a



cleanEx()
nameEx("fold_left")
### * fold_left

flush(stderr()); flush(stdout())

### Name: fold_left
### Title: Fold Left Over a Sequence
### Aliases: fold_left

### ** Examples

t <- as_flexseq(1:5)
sum_m <- measure_monoid(`+`, 0, as.numeric)
fold_left(t, sum_m)

cat_m <- measure_monoid(paste0, "", as.character)
fold_left(as_flexseq(letters[1:4]), cat_m)



cleanEx()
nameEx("fold_right")
### * fold_right

flush(stderr()); flush(stdout())

### Name: fold_right
### Title: Fold Right Over a Sequence
### Aliases: fold_right

### ** Examples

t <- as_flexseq(1:5)
sum_m <- measure_monoid(`+`, 0, as.numeric)
fold_right(t, sum_m)

cat_m <- measure_monoid(paste0, "", as.character)
fold_right(as_flexseq(letters[1:4]), cat_m)



cleanEx()
nameEx("get_graph_df")
### * get_graph_df

flush(stderr()); flush(stdout())

### Name: get_graph_df
### Title: Build graph data frames for a finger tree
### Aliases: get_graph_df
### Keywords: internal

### ** Examples

## Not run: 
##D t <- as_flexseq(letters[1:4])
##D gdf <- get_graph_df(t)
##D names(gdf)
## End(Not run)



cleanEx()
nameEx("insert")
### * insert

flush(stderr()); flush(stdout())

### Name: insert
### Title: Insert an element into a priority queue
### Aliases: insert

### ** Examples

q <- priority_queue("a", "b", priorities = c(2, 1))
q2 <- insert(q, "c", priority = 1)
peek_min(q2)



cleanEx()
nameEx("is_empty")
### * is_empty

flush(stderr()); flush(stdout())

### Name: is_empty
### Title: Check whether a priority queue is empty
### Aliases: is_empty

### ** Examples

q <- priority_queue()
is_empty(q)



cleanEx()
nameEx("locate")
### * locate

flush(stderr()); flush(stdout())

### Name: locate
### Title: Locate first predicate flip without reconstructing context trees
### Aliases: locate

### ** Examples

t <- as_flexseq(letters[1:6])
locate(t, function(v) v >= 4, ".size")

# Metadata-rich locate for custom monoid
sum_m <- measure_monoid(`+`, 0, as.numeric)
t2 <- as_flexseq(1:6, monoids = list(sum = sum_m))
locate(t2, function(v) v >= 10, "sum", include_metadata = TRUE)



cleanEx()
nameEx("measure_monoid")
### * measure_monoid

flush(stderr()); flush(stdout())

### Name: measure_monoid
### Title: Construct a Measure Monoid Specification
### Aliases: measure_monoid

### ** Examples

sum_m <- measure_monoid(`+`, 0, as.numeric)
t <- as_flexseq(1:5)
fold_left(t, sum_m)

# Add a custom monoid after construction
nchar_sum <- measure_monoid(`+`, 0, function(el) nchar(as.character(el)))
t2 <- add_monoids(as_flexseq(letters[1:3]), list(nchar_sum = nchar_sum))
attr(t2, "measures")$nchar_sum



cleanEx()
nameEx("peek_max")
### * peek_max

flush(stderr()); flush(stdout())

### Name: peek_max
### Title: Peek maximum-priority element
### Aliases: peek_max

### ** Examples

q <- priority_queue("a", "b", "c", priorities = c(2, 3, 3))
peek_max(q)



cleanEx()
nameEx("peek_min")
### * peek_min

flush(stderr()); flush(stdout())

### Name: peek_min
### Title: Peek minimum-priority element
### Aliases: peek_min

### ** Examples

q <- priority_queue("a", "b", "c", priorities = c(2, 1, 1))
peek_min(q)



cleanEx()
nameEx("plot_tree")
### * plot_tree

flush(stderr()); flush(stdout())

### Name: plot_tree
### Title: Plot a finger tree with igraph
### Aliases: plot_tree
### Keywords: internal

### ** Examples

## Not run: 
##D t <- as_flexseq(letters[1:8])
##D plot_tree(t, title = "Finger tree")
##D 
##D t2 <- as_flexseq(letters[1:12])
##D plot_tree(t2, node_label = "both", vertex.size = 8)
## End(Not run)



cleanEx()
nameEx("predicate")
### * predicate

flush(stderr()); flush(stdout())

### Name: predicate
### Title: Construct a Predicate Function
### Aliases: predicate

### ** Examples

p <- predicate(function(v) v >= 3)
p(2)
p(3)

t <- as_flexseq(letters[1:5])
locate(t, p, ".size")



cleanEx()
nameEx("prepend")
### * prepend

flush(stderr()); flush(stdout())

### Name: prepend
### Title: Prepend an element
### Aliases: prepend

### ** Examples

t <- as_flexseq(letters[2:4])
t2 <- prepend(t, "a")
t2[[1]]

# Compose with append and reduce
cat_m <- measure_monoid(paste0, "", as.character)
fold_left(append(t2, "z"), cat_m)

# Prepend to a named tree with an explicit element name
tn <- as_flexseq(setNames(as.list(2:3), c("b", "c")))
tn2 <- prepend(tn, stats::setNames(1, "a"))
tn2[["a"]]



cleanEx()
nameEx("print.FingerTree")
### * print.FingerTree

flush(stderr()); flush(stdout())

### Name: print.FingerTree
### Title: Print a compact summary of a finger tree
### Aliases: print.FingerTree print.Deep print.Single print.Empty
### Keywords: internal

### ** Examples

t <- as_flexseq(letters[1:10])
t

tn <- as_flexseq(setNames(as.list(1:5), paste0("k", 1:5)))
tn



cleanEx()
nameEx("print.priority_queue")
### * print.priority_queue

flush(stderr()); flush(stdout())

### Name: print.priority_queue
### Title: Print a Priority Queue
### Aliases: print.priority_queue

### ** Examples

q <- priority_queue("a", "b", "c", priorities = c(2, 1, 3))
q



cleanEx()
nameEx("priority_queue")
### * priority_queue

flush(stderr()); flush(stdout())

### Name: priority_queue
### Title: Construct a Priority Queue
### Aliases: priority_queue

### ** Examples

q <- priority_queue("a", "b", "c", priorities = c(2, 1, 2))
peek_min(q)



cleanEx()
nameEx("split.flexseq")
### * split.flexseq

flush(stderr()); flush(stdout())

### Name: split.flexseq
### Title: Split a flexseq into left and right parts
### Aliases: split.flexseq

### ** Examples

t <- as_flexseq(letters[1:6])
s <- split(t, function(v) v >= 4, ".size")
length(s$left)
length(s$right)



cleanEx()
nameEx("split_tree")
### * split_tree

flush(stderr()); flush(stdout())

### Name: split_tree
### Title: Split tree around first predicate flip
### Aliases: split_tree

### ** Examples

t <- as_flexseq(letters[1:6])
s <- split_tree(t, function(v) v >= 4, ".size")
s$elem

cat_m <- measure_monoid(paste0, "", as.character)
fold_left(s$left, cat_m)
fold_left(s$right, cat_m)



cleanEx()
nameEx("sub-.FingerTree")
### * sub-.FingerTree

flush(stderr()); flush(stdout())

### Name: [.FingerTree
### Title: Subset a finger tree by position or element name
### Aliases: [.FingerTree

### ** Examples

t <- as_flexseq(letters[1:6])
s <- t[c(2, 4, 6)]
cat_m <- measure_monoid(paste0, "", as.character)
fold_left(s, cat_m)

# Empty index returns empty tree
attr(t[integer(0)], "measures")$.size

# Character indexing by element names
tn <- as_flexseq(setNames(as.list(letters[1:4]), c("w", "x", "y", "z")))
out <- tn[c("y", "missing", "w")]
fold_left(out, measure_monoid(paste0, "", function(el) if(is.null(el)) "_" else el))

# Logical indexing with recycling
t[c(TRUE, FALSE)]



cleanEx()
nameEx("sub-sub-.FingerTree")
### * sub-sub-.FingerTree

flush(stderr()); flush(stdout())

### Name: [[.FingerTree
### Title: Extract one element by position or unique name
### Aliases: [[.FingerTree

### ** Examples

t <- as_flexseq(letters[1:5])
t[[3]]

tn <- as_flexseq(setNames(as.list(letters[1:3]), c("a1", "a2", "a3")))
tn[["a2"]]



cleanEx()
nameEx("sub-subset-.FingerTree")
### * sub-subset-.FingerTree

flush(stderr()); flush(stdout())

### Name: [[<-.FingerTree
### Title: Replace one element by position or unique name
### Aliases: [[<-.FingerTree

### ** Examples

t <- as_flexseq(letters[1:4])
t[[2]] <- "ZZ"
cat_m <- measure_monoid(paste0, "", as.character)
fold_left(t, cat_m)

tn <- as_flexseq(setNames(as.list(1:3), c("x", "y", "z")))
tn[["y"]] <- 99

# Assigning NULL removes an element (by index or name)
t <- as_flexseq(letters[1:4])
t[[2]] <- NULL
tn <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
tn[["b"]] <- NULL



cleanEx()
nameEx("subset-.FingerTree")
### * subset-.FingerTree

flush(stderr()); flush(stdout())

### Name: [<-.FingerTree
### Title: Replace selected elements by position or name
### Aliases: [<-.FingerTree

### ** Examples

t <- as_flexseq(1:6)
t[c(2, 5)] <- list(20, 50)
sum_m <- measure_monoid(`+`, 0, as.numeric)
fold_left(t, sum_m)

# Replacement length must match
try(t[c(1, 2)] <- list(999))

# Character replacement by element names (missing names error)
tn <- as_flexseq(setNames(as.list(1:4), c("a", "b", "c", "d")))
tn[c("d", "a")] <- list(40, 10)

# Logical replacement with recycling
t[c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)] <- list(1)



cleanEx()
nameEx("tree_from")
### * tree_from

flush(stderr()); flush(stdout())

### Name: tree_from
### Title: Build a Structural Tree from a Vector or List
### Aliases: tree_from
### Keywords: internal

### ** Examples

## Not run: 
##D t <- tree_from(letters[1:4])
##D t[[2]]
## End(Not run)



cleanEx()
nameEx("validate_name_state")
### * validate_name_state

flush(stderr()); flush(stdout())

### Name: validate_name_state
### Title: Validate name-state invariants only (debug/test utility)
### Aliases: validate_name_state

### ** Examples

t <- as_flexseq(setNames(as.list(letters[1:4]), letters[1:4]))
validate_name_state(t)



cleanEx()
nameEx("validate_tree")
### * validate_tree

flush(stderr()); flush(stdout())

### Name: validate_tree
### Title: Validate full tree invariants (debug/test utility)
### Aliases: validate_tree

### ** Examples

t <- as_flexseq(letters[1:10])
validate_tree(t)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
