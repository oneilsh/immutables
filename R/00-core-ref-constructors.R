#SO

##############################
## Node type definitions
##############################

# generic node type
# stores arbitrary child elements in a list; base for Node2/Node3
# Runtime: O(k), where k is number of constructor arguments.
Node(...) %::% ... : list
if(FALSE) Node <- function(...) NULL
Node(...) %as% { 
  res <- list(...)
  return(res)
}

# Node2 and Node3 node types
# represent internal nodes with 2 or 3 children
# Runtime: O(1).
Node2(x, y) %::% . : . : list
if(FALSE) Node2 <- function(x, y) NULL
Node2(x, y) %as% Node(x, y)

# Runtime: O(1).
Node3(x, y, z) %::% . : . : . : list
if(FALSE) Node3 <- function(x, y, z) NULL
Node3(x, y, z) %as% Node(x, y, z)


# Runtime: O(1).
FingerTree() %::% FingerTree
if(FALSE) FingerTree <- function() NULL
FingerTree() %as% Empty()


# basic constructor for inheriting
# internal convenience for creating tree records
# Runtime: O(k), where k is number of constructor arguments.
FingerTree(...) %::% ... : list
if(FALSE) FingerTree <- function(...) NULL
FingerTree(...) %as% {
  list(...)
}

# empty node type
# represents an empty tree
# Runtime: O(1).
Empty() %::% FingerTree
if(FALSE) Empty <- function() NULL
Empty() %as% FingerTree(NULL)


# single-element node type
# represents a tree with exactly one element
# Runtime: O(1).
if(FALSE) Single <- function() NULL
Single(x) %::% . : FingerTree
if(FALSE) Single <- function(x) NULL
Single(x) %as% FingerTree(x)

# digits are like nodes, but they allow 1 to 4 elements
# used as prefix/suffix containers in Deep
# Runtime: O(k), where k is number of digit elements.
Digit(...) %::% ... : list
if(FALSE) Digit <- function(...) NULL
Digit(...) %as% {
  list(...)
}

# Deep is the main data type, with a prefix (digit), middle (fingertree), and suffix (digit)
# Runtime: O(1).
Deep(prefix, middle, suffix) %::% Digit : FingerTree : Digit : FingerTree
if(FALSE) Deep <- function(prefix, middle, suffix) NULL
Deep(prefix, middle, suffix) %as% {
  FingerTree(prefix = prefix, middle = middle, suffix = suffix)   
}
