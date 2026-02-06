
##############################
## Node type definitions
##############################

# generic node type
Node(...) %::% ... : list
Node(...) %as% { 
  res <- list(...)
  return(res)
}

# Node2 and Node3 node types
Node2(x, y) %::% a : a : list
Node2(x, y) %as% Node(x, y)

Node3(x, y, z) %::% a : a : a : list
Node3(x, y, z) %as% Node(x, y, z)


FingerTree() %::% FingerTree
FingerTree() %as% Empty()


# basic constructor for inheriting (this should probably just inherit from Node, or others should directly
# inherit from node rather than this)
FingerTree(...) %::% ... : list
FingerTree(...) %as% {
  list(...)
}

# empty node type
Empty() %::% FingerTree
Empty() %as% FingerTree(NULL)


# single-element node type
Single(x) %::% . : FingerTree
Single(x) %as% FingerTree(x)

# digits are like nodes, but they allow 1 to 4 elements
Digit(...) %::% ... : list
Digit(...) %as% {
  list(...)
}

# Deep is the main data type, with a prefix (digit), middle (fingertree of some type, either empty, single, or deep),
# and a suffix (digit)
Deep(prefix, middle, suffix) %::% Digit : FingerTree : Digit : FingerTree
Deep(prefix, middle, suffix) %as% {
  FingerTree(prefix = prefix, middle = middle, suffix = suffix)   
}



##############################
## Monoid-annotation
## (actually, generalized, "reducer" annotation, and reduction functions that apply monoids (sorry, reducers),
## to sequences of tags, either from the left or the right)
##############################

# A reducer is a generalized monoid, since it's not actually required that the
# function be fully associative (which thus allows a "reduce_left" and "reduce_right" depending on
# if we want to treat it as left-associative or right-associative)
Reducer(f, i) %::% . : . : list
Reducer(f, i) %as% {
  list(f = f, i = i)
}


# in order to convert the tree to an igraph object, we need an obvious way to determine that
# the node is an element (non-recursive case); this could also probably inherit from Node to grab it's random id
Element(x) %::% . : .
Element(x, value = x) %as% {
  res <- x
  # @ is used to set attributes (in lambda.r constructors)
  res@value <- value
  return(res)
}

print.Element <- function(e) {
  #cat("Data Element: \n")
  ecopy <- e
  class(ecopy) <- class(e)[class(e) != "Element"]
  attr(ecopy, "value") <- NULL
  #print(ecopy)
  if(!is.null(attr(e, "value"))) {
    cat("Value: \n")
    print(attr(e, "value"))
    cat("\n")
  }
  cat("\n")
}


Predicate(f) %::% Function : .
Predicate(f) %as% Function(f)


  
  
