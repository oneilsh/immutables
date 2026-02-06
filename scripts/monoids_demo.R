# This script explores monoid-annotated binary trees with elements stored in leaves. 


### First up, a monoid is a data type containing a function
### (which should be associative binary) and an identity element:
monoid <- function(f, i) {
  res <- list(f = f, i = i)
  class(res) <- "monoid"
  return(res)
}

adder <- monoid(`+`, 0)
multiplier <- monoid(`*`, 1)
minner <- monoid(min, Inf)
maxxer <- monoid(max, -Inf)
catter <- monoid(paste0, "")
righter <- monoid(function(x, y) {return(y)}, "NoKey")

### A "fold" is repeated application of the function from a monoid
### to a list of elements
### since the monoid is associative, it doesn't matter if it's e.g.
### (e1 + (e2 + (e3 + i))) or (((i + e1) + e2) + e3)
fold <- function(inputlist, monoid) {
  res <- monoid$i
  for(el in inputlist) {
    res <- monoid$f(res, el)
  }
  return(res)
}


nums <- list(3, 4, 2, 5, 3)
print(fold(nums, adder)) # sum them up         - 17
print(fold(nums, multiplier)) # mult them up   - 360
print(fold(nums, minner)) # min them up        - 2

## since the monoid insists on having an identity element, they work on empty lists too:
print(fold(list(), adder)) # sum them up       - 0
print(fold(list(), multiplier)) # mult them up - 1
print(fold(list(), minner)) # min them up      - Inf

##############
### Monoids applied to trees: see https://apfelmus.nfshost.com/articles/monoid-fingertree.html
##############
library(TurtleGraphics)

turtle_getstate <- function() {
  state <- c(turtle_getpos(), turtle_getangle())
  return(state)
}
turtle_setstate <- function(state) {
  turtle_setpos(state[1], state[2])
  turtle_setangle(state[3])
}
turtle_text <- function(label, col = "black", fontsize = 8, readable = FALSE) {
  rot <- -1*turtle_getangle()
  if(readable) {rot <- 0}
  grid.text(label,
            turtle_getpos()[1],
            turtle_getpos()[2],
            rot = rot,
            default.units = "native",
            gp = gpar(fontsize = fontsize, col = col, lineheight = 0.7))
}

draw_tree <- function(t) {
  turtle_init(mode = "clip")
  turtle_hide()
  turtle_setstate(c(50, 90, 180))
  turtle_col("gray")
  if(is.null(t)) {
    turtle_text("~Empty Tree~", readable = TRUE)
  } else {
    draw_tree_internal(t)
  }
}

draw_tree_internal <- function(t) {
  s <- turtle_getstate()
  if(class(t) == "leaf") {
    turtle_text(t$tag, col = "red", readable = TRUE)
    turtle_up()
    turtle_forward(3)
    turtle_down()
    turtle_text(t$el, readable = TRUE)
  } else {
    # we're upside down, so turn right to draw left...
    turtle_right(15 + 2 * count_nodes(t$left))
    turtle_forward(10)
    draw_tree_internal(t$left)
    turtle_setstate(s)
    
    turtle_left(10 * count_nodes(t$right))
    turtle_forward(10)
    draw_tree_internal(t$right)
    turtle_setstate(s)
    turtle_text(t$tag, col = "red", readable = TRUE)
    
  }
  
  turtle_setstate(s)
}

count_nodes <- function(t) {
  if(class(t) == "leaf") {
    return(1)
  }
  return(count_nodes(t$left) + count_nodes(t$right))
}

leaf <- function(el, tag) {
  res <- list(el = el, tag = tag)
  class(res) <- "leaf"
  return(res)
}


# left and right must be either a leaf, or another tree (so we know it has a tag)
tree <- function(left, right, tagger) {
  tag <- tagger$f(left$tag, right$tag)
  res <- list(left = left, right = right, tag = tag)
  class(res) <- "tree"
  return(res)
}


insert_tree <- function(t, el, tag, tagger) {
  if(is.null(t)) {
    return(leaf(el, tag))
  }
  
  if(class(t) == "leaf") {
    newleaf <- leaf(el, tag)
    newtree <- tree(t, newleaf, tagger)
    return(newtree)
  }
 
  if(runif(1) < 0.5) {
    newleft <- insert_tree(t$left, el, tag, tagger)
    newtree <- tree(newleft, t$right, tagger)
    return(newtree)
  } else {
    newright <- insert_tree(t$right, el, tag, tagger)
    newtree <- tree(t$left, newright, tagger)
  }
}

# do the functions really need to be monoids? It seems we're *just* using the function in 
# them
t1 <- NULL
for(randindex in sample(1:26, 10)) {
  # prio-queue
  #t1 <- insert_tree(t1, letters[randindex], randindex, minner)
  
  # get-nth tree
  #t1 <- insert_tree(t1, letters[randindex], 1, adder)
  
  # concatenated string tree... ?
  #t1 <- insert_tree(t1, letters[randindex], letters[randindex], catter)
  
  # search tree... ?
  t1 <- insert_tree(t1, letters[randindex], letters[randindex], righter)
  
}


draw_tree(t1)
