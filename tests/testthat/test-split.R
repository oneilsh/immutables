testthat::test_that("split_tree returns distinguished element and context", {
  mr <- MeasureReducer(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:6], reducer = mr)

  s <- split_tree(t, function(v) v >= 4)
  testthat::expect_identical(s$elem, "d")
  testthat::expect_identical(attr(s$left, "measure"), 3)
  testthat::expect_identical(attr(s$right, "measure"), 2)
})

testthat::test_that("split returns left/right trees with split element prepended to right", {
  mr <- MeasureReducer(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:6], reducer = mr)

  s <- split(t, function(v) v >= 4)
  testthat::expect_identical(attr(s$left, "measure"), 3)
  testthat::expect_identical(attr(s$right, "measure"), 3)
  testthat::expect_identical(reduce_left(s$left, Reducer(function(a, b) paste0(a, b), "")), "abc")
  testthat::expect_identical(reduce_left(s$right, Reducer(function(a, b) paste0(a, b), "")), "def")
})

testthat::test_that("split handles empty and predicate-never-true cases", {
  mr <- MeasureReducer(function(a, b) a + b, 0, function(el) 1)
  e <- empty_tree(reducer = mr)
  s0 <- split(e, function(v) v >= 1)
  testthat::expect_true(s0$left %isa% Empty)
  testthat::expect_true(s0$right %isa% Empty)

  t <- tree_from(letters[1:4], reducer = mr)
  s <- split(t, function(v) v >= 10)
  testthat::expect_identical(attr(s$left, "measure"), 4)
  testthat::expect_true(s$right %isa% Empty)
})
