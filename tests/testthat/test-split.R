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

testthat::test_that("split boundary at first element keeps full right side", {
  mr <- MeasureReducer(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:5], reducer = mr)
  s <- split(t, function(v) v >= 1)

  testthat::expect_true(s$left %isa% Empty)
  testthat::expect_identical(attr(s$right, "measure"), 5)
  testthat::expect_identical(reduce_left(s$right, Reducer(function(a, b) paste0(a, b), "")), "abcde")
})

testthat::test_that("split boundary at last element keeps single-element right side", {
  mr <- MeasureReducer(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:5], reducer = mr)
  s <- split(t, function(v) v >= 5)

  testthat::expect_identical(attr(s$left, "measure"), 4)
  testthat::expect_identical(attr(s$right, "measure"), 1)
  testthat::expect_identical(reduce_left(s$left, Reducer(function(a, b) paste0(a, b), "")), "abcd")
  testthat::expect_identical(reduce_left(s$right, Reducer(function(a, b) paste0(a, b), "")), "e")
})

testthat::test_that("split_tree respects custom accumulator offsets", {
  mr <- MeasureReducer(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:5], reducer = mr)

  s <- split_tree(t, function(v) v >= 8, accumulator = 5)
  testthat::expect_identical(s$elem, "c")
  testthat::expect_identical(attr(s$left, "measure"), 2)
  testthat::expect_identical(attr(s$right, "measure"), 2)
})

testthat::test_that("split_tree edge behavior on single and invalid precondition", {
  mr <- MeasureReducer(function(a, b) a + b, 0, function(el) 1)
  one <- tree_from("x", reducer = mr)
  s1 <- split_tree(one, function(v) v >= 1)
  testthat::expect_true(s1$left %isa% Empty)
  testthat::expect_identical(s1$elem, "x")
  testthat::expect_true(s1$right %isa% Empty)

  t <- tree_from(letters[1:4], reducer = mr)
  testthat::expect_error(split_tree(t, function(v) v >= 10), "predicate never became true")
})

testthat::test_that("split and split_tree require MeasureReducer", {
  r <- Reducer(function(a, b) a + b, 0)
  t <- tree_from(1:4, reducer = r)
  testthat::expect_error(split(t, function(v) v >= 2), "MeasureReducer")
  testthat::expect_error(split_tree(t, function(v) v >= 2), "MeasureReducer")
})
