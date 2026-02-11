
##############################
## Node type definitions
##############################

# generic node type
# stores arbitrary child elements in a list; base for Node2/Node3
Node(...) %::% ... : list
Node(...) %as% { 
  res <- list(...)
  return(res)
}

# Node2 and Node3 node types
# represent internal nodes with 2 or 3 children
Node2(x, y) %::% a : a : list
Node2(x, y) %as% Node(x, y)

Node3(x, y, z) %::% a : a : a : list
Node3(x, y, z) %as% Node(x, y, z)


FingerTree() %::% FingerTree
FingerTree() %as% Empty()


# basic constructor for inheriting
# internal convenience for creating tree records
FingerTree(...) %::% ... : list
FingerTree(...) %as% {
  list(...)
}

# empty node type
# represents an empty tree
Empty() %::% FingerTree
Empty() %as% FingerTree(NULL)


# single-element node type
# represents a tree with exactly one element
Single(x) %::% . : FingerTree
Single(x) %as% FingerTree(x)

# digits are like nodes, but they allow 1 to 4 elements
# used as prefix/suffix containers in Deep
Digit(...) %::% ... : list
Digit(...) %as% {
  list(...)
}

# Deep is the main data type, with a prefix (digit), middle (fingertree), and suffix (digit)
Deep(prefix, middle, suffix) %::% Digit : FingerTree : Digit : FingerTree
Deep(prefix, middle, suffix) %as% {
  FingerTree(prefix = prefix, middle = middle, suffix = suffix)   
}
