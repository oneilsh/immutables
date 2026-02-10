# Load package code when tests are run ad hoc via testthat::test_dir().
# testthat::test_check() loads the installed package and does not need this.
if (!exists("tree_from", mode = "function")) {
  suppressPackageStartupMessages(library(lambda.r))
  if (!exists("list.prepend", mode = "function")) {
    list.prepend <- function(x, value) c(list(value), x)
  }
  if (!exists("list.append", mode = "function")) {
    list.append <- function(x, value) c(x, list(value))
  }
  r_files <- sort(list.files("R", pattern = "[.][Rr]$", full.names = TRUE))
  for (f in r_files) {
    source(f, local = .GlobalEnv)
  }
}
