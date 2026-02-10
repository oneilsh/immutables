testthat::test_that("split_digit handles first/middle/last boundaries", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  d <- Digit("a", "b", "c")

  s1 <- fingertree:::split_digit(function(v) v >= 1, 0, d, mr)
  testthat::expect_identical(s1$elem, "a")
  testthat::expect_identical(unclass(s1$left), list())
  testthat::expect_identical(unclass(s1$right), list("b", "c"))

  s2 <- fingertree:::split_digit(function(v) v >= 2, 0, d, mr)
  testthat::expect_identical(s2$elem, "b")
  testthat::expect_identical(unclass(s2$left), list("a"))
  testthat::expect_identical(unclass(s2$right), list("c"))

  s3 <- fingertree:::split_digit(function(v) v >= 3, 0, d, mr)
  testthat::expect_identical(s3$elem, "c")
  testthat::expect_identical(unclass(s3$left), list("a", "b"))
  testthat::expect_identical(unclass(s3$right), list())
})

testthat::test_that("split_digit works when digit holds nodes", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  n2 <- Node2("a", "b")
  n3 <- Node3("c", "d", "e")
  d <- Digit(n2, n3)

  s <- fingertree:::split_digit(function(v) v >= 3, 0, d, mr)
  testthat::expect_true(s$elem %isa% Node)
  testthat::expect_identical(length(s$left), 1L)
  testthat::expect_identical(length(s$right), 0L)
})

testthat::test_that("split_digit errors for invalid preconditions", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  testthat::expect_error(
    fingertree:::split_digit(function(v) v >= 1, 0, list(), mr),
    "empty digit"
  )

  d <- Digit("a", "b")
  testthat::expect_error(
    fingertree:::split_digit(function(v) v >= 10, 0, d, mr),
    "predicate never became true"
  )
})
