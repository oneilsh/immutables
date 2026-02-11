testthat::test_that("split_tree returns distinguished element and context", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:6], monoids = list(count = mr))

  s <- split_tree(t, function(v) v >= 4, ".size")
  testthat::expect_identical(s$elem, "d")
  testthat::expect_identical(attr(s$left, "measures")$.size, 3)
  testthat::expect_identical(attr(s$right, "measures")$.size, 2)
})

testthat::test_that("split returns left/right trees with split element prepended to right", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  sr <- MeasureMonoid(function(a, b) paste0(a, b), "", function(el) "")
  t <- tree_from(letters[1:6], monoids = list(count = mr))

  s <- split(t, function(v) v >= 4, ".size")
  testthat::expect_identical(attr(s$left, "measures")$.size, 3)
  testthat::expect_identical(attr(s$right, "measures")$.size, 3)
  testthat::expect_identical(reduce_left(s$left, sr), "abc")
  testthat::expect_identical(reduce_left(s$right, sr), "def")
})

testthat::test_that("split handles empty and predicate-never-true cases", {
  e <- empty_tree()
  s0 <- split(e, function(v) v >= 1, ".size")
  testthat::expect_true(s0$left %isa% Empty)
  testthat::expect_true(s0$right %isa% Empty)

  t <- tree_from(letters[1:4])
  s <- split(t, function(v) v >= 10, ".size")
  testthat::expect_identical(attr(s$left, "measures")$.size, 4)
  testthat::expect_true(s$right %isa% Empty)
})

testthat::test_that("split boundary at first and last element", {
  sr <- MeasureMonoid(function(a, b) paste0(a, b), "", function(el) "")
  t <- tree_from(letters[1:5])

  s1 <- split(t, function(v) v >= 1, ".size")
  testthat::expect_true(s1$left %isa% Empty)
  testthat::expect_identical(attr(s1$right, "measures")$.size, 5)
  testthat::expect_identical(reduce_left(s1$right, sr), "abcde")

  s2 <- split(t, function(v) v >= 5, ".size")
  testthat::expect_identical(attr(s2$left, "measures")$.size, 4)
  testthat::expect_identical(attr(s2$right, "measures")$.size, 1)
  testthat::expect_identical(reduce_left(s2$left, sr), "abcd")
  testthat::expect_identical(reduce_left(s2$right, sr), "e")
})

testthat::test_that("split_tree respects custom accumulator offsets", {
  t <- tree_from(letters[1:5])

  s <- split_tree(t, function(v) v >= 8, ".size", accumulator = 5)
  testthat::expect_identical(s$elem, "c")
  testthat::expect_identical(attr(s$left, "measures")$.size, 2)
  testthat::expect_identical(attr(s$right, "measures")$.size, 2)
})

testthat::test_that("split_tree errors when monoid name is missing", {
  t <- tree_from(letters[1:4])
  testthat::expect_error(split_tree(t, function(v) v >= 2, "unknown"), "not found")
})
