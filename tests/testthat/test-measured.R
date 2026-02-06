testthat::test_that("Measured reducer caches size measures", {
  mr <- MeasuredReducer(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(1:5, reducer = mr)

  testthat::expect_identical(attr(t, "measure"), 5)
  testthat::expect_identical(reduce_left(t), 15)
})

testthat::test_that("Measured reducer propagates through prepend/append", {
  mr <- MeasuredReducer(function(a, b) a + b, 0, function(el) 1)
  t <- empty_tree(reducer = mr)
  t <- append(t, "a")
  t <- append(t, "b")
  t <- prepend(t, "z")

  testthat::expect_identical(attr(t, "measure"), 3)
})

testthat::test_that("Measured reducer works with list elements", {
  mr <- MeasuredReducer(function(a, b) a + b, 0, function(el) length(el))
  t <- empty_tree(reducer = mr)
  t <- append(t, list(1, 2, 3))
  t <- append(t, list("a"))
  t <- append(t, list(TRUE, FALSE))

  testthat::expect_identical(attr(t, "measure"), 6)
})
