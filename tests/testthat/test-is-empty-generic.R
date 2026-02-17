testthat::test_that("is_empty dispatches for flexseq", {
  testthat::expect_true(is_empty(flexseq()))
  testthat::expect_false(is_empty(as_flexseq(1:3)))
})

testthat::test_that("is_empty dispatches for ordered_multiset", {
  ms0 <- as_ordered_multiset(list(), keys = NULL)
  ms1 <- as_ordered_multiset(list("a"), keys = 1)
  testthat::expect_true(is_empty(ms0))
  testthat::expect_false(is_empty(ms1))
})

testthat::test_that("is_empty default errors on unsupported class", {
  testthat::expect_error(is_empty(1:3), "No `is_empty()` method", fixed = TRUE)
})
