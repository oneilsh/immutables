is_structural_node <- function(x) {
  x %isa% FingerTree || x %isa% Deep || x %isa% Digit || x %isa% Node || x %isa% Single || x %isa% Empty
}
