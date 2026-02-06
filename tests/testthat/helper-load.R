testthat::skip_if_not_installed("lambda.r")
testthat::skip_if_not_installed("rlist")
testthat::skip_if_not_installed("waldo")

library(lambda.r)
library(rlist)
library(waldo)

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
test_env <- parent.frame()
for (f in sort(r_files)) {
  source(f, local = test_env)
}
