testthat::test_that("Elements can be lists and data.frames", {
  r <- measure_monoid(function(a, b) c(a, list(b)), list(), function(el) 0)
  df <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
  t <- flexseq()
  t <- append(t, list(a = 1, b = 2))
  t <- append(t, df)

  reduced <- fold_left(t, r)
  testthat::expect_identical(length(reduced), 2L)
  testthat::expect_true(is.list(reduced[[1]]))
  testthat::expect_true(is.data.frame(reduced[[2]]))
})
