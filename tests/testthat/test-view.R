tree_chars <- function(t) {
  r <- MeasureMonoid(function(a, b) paste0(a, b), "", function(el) "")
  reduce_left(t, r)
}

testthat::test_that("viewL and viewR return boundary element and remainder", {
  m_count <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:5], monoids = list(count = m_count))
  ms <- attr(t, "monoids")

  vl <- fingertree:::viewL(t, ms)
  testthat::expect_identical(vl$elem, "a")
  testthat::expect_identical(tree_chars(vl$rest), "bcde")

  vr <- fingertree:::viewR(t, ms)
  testthat::expect_identical(vr$elem, "e")
  testthat::expect_identical(tree_chars(vr$rest), "abcd")
})

testthat::test_that("viewL/viewR on Single produce Empty remainder", {
  t <- tree_from("x")
  ms <- attr(t, "monoids")

  vl <- fingertree:::viewL(t, ms)
  vr <- fingertree:::viewR(t, ms)

  testthat::expect_identical(vl$elem, "x")
  testthat::expect_identical(vr$elem, "x")
  testthat::expect_true(vl$rest %isa% Empty)
  testthat::expect_true(vr$rest %isa% Empty)
})

testthat::test_that("deepL/deepR collapse correctly when middle is Empty", {
  ms <- ensure_size_monoids(list(.size = size_measure_monoid()))
  pr <- list()
  sf <- Digit("c", "d")
  t1 <- fingertree:::deepL(pr, measured_empty(ms), sf, ms)
  testthat::expect_identical(tree_chars(t1), "cd")

  pr2 <- Digit("a", "b")
  sf2 <- list()
  t2 <- fingertree:::deepR(pr2, measured_empty(ms), sf2, ms)
  testthat::expect_identical(tree_chars(t2), "ab")
})

testthat::test_that("measured view helpers preserve measures attrs", {
  t <- tree_from(letters[1:4])
  ms <- attr(t, "monoids")

  vl <- fingertree:::viewL(t, ms)
  vr <- fingertree:::viewR(t, ms)

  testthat::expect_identical(attr(vl$rest, "measures")$.size, 3)
  testthat::expect_identical(attr(vr$rest, "measures")$.size, 3)
})
