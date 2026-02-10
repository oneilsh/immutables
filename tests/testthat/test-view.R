tree_chars <- function(t) {
  reduce_left(t, Monoid(function(a, b) paste0(a, b), ""))
}

testthat::test_that("viewL and viewR return boundary element and remainder", {
  t <- tree_from(letters[1:5])

  vl <- fingertree:::viewL(t)
  testthat::expect_identical(vl$elem, "a")
  testthat::expect_identical(tree_chars(vl$rest), "bcde")

  vr <- fingertree:::viewR(t)
  testthat::expect_identical(vr$elem, "e")
  testthat::expect_identical(tree_chars(vr$rest), "abcd")
})

testthat::test_that("viewL/viewR on Single produce Empty remainder", {
  t <- tree_from("x")

  vl <- fingertree:::viewL(t)
  vr <- fingertree:::viewR(t)

  testthat::expect_identical(vl$elem, "x")
  testthat::expect_identical(vr$elem, "x")
  testthat::expect_true(vl$rest %isa% Empty)
  testthat::expect_true(vr$rest %isa% Empty)
})

testthat::test_that("deepL/deepR collapse correctly when middle is Empty", {
  pr <- list()
  sf <- Digit("c", "d")
  t1 <- fingertree:::deepL(pr, Empty(), sf)
  testthat::expect_identical(tree_chars(t1), "cd")

  pr2 <- Digit("a", "b")
  sf2 <- list()
  t2 <- fingertree:::deepR(pr2, Empty(), sf2)
  testthat::expect_identical(tree_chars(t2), "ab")
})

testthat::test_that("measured view helpers preserve measure attributes", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:4], monoid = mr)

  vl <- fingertree:::viewL(t, mr)
  vr <- fingertree:::viewR(t, mr)

  testthat::expect_identical(attr(vl$rest, "measure"), 3)
  testthat::expect_identical(attr(vr$rest, "measure"), 3)
})
