testthat::test_that("Elements can be lists and data.frames", {
  df <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
  payload <- list(a = 1, b = 2)
  t <- flexseq()
  t <- push_back(t, payload)
  t <- push_back(t, df)

  reduced <- as.list(t)
  testthat::expect_identical(length(reduced), 2L)
  testthat::expect_true(is.list(reduced[[1]]))
  testthat::expect_true(is.data.frame(reduced[[2]]))
  testthat::expect_identical(reduced[[1]], payload)
  testthat::expect_identical(reduced[[2]], df)
})
