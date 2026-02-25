#SO

# identify internal structural nodes (vs user elements)
# Runtime: O(1).
if(FALSE) is_structural_node <- function(x) NULL
is_structural_node(x) %::% . : logical
is_structural_node(x) %as% {
  x %isa% FingerTree || x %isa% Deep || x %isa% Digit || x %isa% Node || x %isa% Single || x %isa% Empty
}
