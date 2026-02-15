# identify internal structural nodes (vs user elements)
# Runtime: O(n) worst-case in relevant input/subtree size.
is_structural_node(x) %::% . : logical
is_structural_node(x) %as% {
  x %isa% FingerTree || x %isa% Deep || x %isa% Digit || x %isa% Node || x %isa% Single || x %isa% Empty
}
