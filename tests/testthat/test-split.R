testthat::test_that("split_tree returns distinguished element and context", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:6], monoid = mr)

  s <- split_tree(t, function(v) v >= 4)
  testthat::expect_identical(s$elem, "d")
  testthat::expect_identical(attr(s$left, "measure"), 3)
  testthat::expect_identical(attr(s$right, "measure"), 2)
})

testthat::test_that("split returns left/right trees with split element prepended to right", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:6], monoid = mr)

  s <- split(t, function(v) v >= 4)
  testthat::expect_identical(attr(s$left, "measure"), 3)
  testthat::expect_identical(attr(s$right, "measure"), 3)
  testthat::expect_identical(reduce_left(s$left, Monoid(function(a, b) paste0(a, b), "")), "abc")
  testthat::expect_identical(reduce_left(s$right, Monoid(function(a, b) paste0(a, b), "")), "def")
})

testthat::test_that("split handles empty and predicate-never-true cases", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  e <- empty_tree(monoid = mr)
  s0 <- split(e, function(v) v >= 1)
  testthat::expect_true(s0$left %isa% Empty)
  testthat::expect_true(s0$right %isa% Empty)

  t <- tree_from(letters[1:4], monoid = mr)
  s <- split(t, function(v) v >= 10)
  testthat::expect_identical(attr(s$left, "measure"), 4)
  testthat::expect_true(s$right %isa% Empty)
})

testthat::test_that("split boundary at first element keeps full right side", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:5], monoid = mr)
  s <- split(t, function(v) v >= 1)

  testthat::expect_true(s$left %isa% Empty)
  testthat::expect_identical(attr(s$right, "measure"), 5)
  testthat::expect_identical(reduce_left(s$right, Monoid(function(a, b) paste0(a, b), "")), "abcde")
})

testthat::test_that("split boundary at last element keeps single-element right side", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:5], monoid = mr)
  s <- split(t, function(v) v >= 5)

  testthat::expect_identical(attr(s$left, "measure"), 4)
  testthat::expect_identical(attr(s$right, "measure"), 1)
  testthat::expect_identical(reduce_left(s$left, Monoid(function(a, b) paste0(a, b), "")), "abcd")
  testthat::expect_identical(reduce_left(s$right, Monoid(function(a, b) paste0(a, b), "")), "e")
})

testthat::test_that("split_tree respects custom accumulator offsets", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:5], monoid = mr)

  s <- split_tree(t, function(v) v >= 8, accumulator = 5)
  testthat::expect_identical(s$elem, "c")
  testthat::expect_identical(attr(s$left, "measure"), 2)
  testthat::expect_identical(attr(s$right, "measure"), 2)
})

testthat::test_that("split_tree edge behavior on single and invalid precondition", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  one <- tree_from("x", monoid = mr)
  s1 <- split_tree(one, function(v) v >= 1)
  testthat::expect_true(s1$left %isa% Empty)
  testthat::expect_identical(s1$elem, "x")
  testthat::expect_true(s1$right %isa% Empty)

  t <- tree_from(letters[1:4], monoid = mr)
  testthat::expect_error(split_tree(t, function(v) v >= 10), "predicate never became true")
})

testthat::test_that("split and split_tree require MeasureMonoid", {
  r <- Monoid(function(a, b) a + b, 0)
  t <- tree_from(1:4, monoid = r)
  testthat::expect_error(split(t, function(v) v >= 2), "MeasureMonoid")
  testthat::expect_error(split_tree(t, function(v) v >= 2), "MeasureMonoid")
})
