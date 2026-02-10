testthat::test_that("Measured monoid caches size measures", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(1:5, monoid = mr)

  testthat::expect_identical(attr(t, "measure"), 5)
  testthat::expect_identical(reduce_left(t), 15)
})

testthat::test_that("Measured monoid propagates through prepend/append", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- empty_tree(monoid = mr)
  t <- append(t, "a")
  t <- append(t, "b")
  t <- prepend(t, "z")

  testthat::expect_identical(attr(t, "measure"), 3)
})

testthat::test_that("Measured monoid works with list elements", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) length(el))
  t <- empty_tree(monoid = mr)
  t <- append(t, list(1, 2, 3))
  t <- append(t, list("a"))
  t <- append(t, list(TRUE, FALSE))

  testthat::expect_identical(attr(t, "measure"), 6)
})

testthat::test_that("Multiple measures are cached on structural nodes", {
  m_sum <- MeasureMonoid(function(a, b) a + b, 0, function(el) el)
  m_size <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(1:5, monoid = list(sum = m_sum, .size = m_size))

  ms <- attr(t, "measures")
  testthat::expect_identical(ms$sum, 15)
  testthat::expect_identical(ms$.size, 5)
  testthat::expect_identical(attr(t, "measure"), 15)
  testthat::expect_identical(reduce_left(t), 15)
})

testthat::test_that("Multiple measures propagate through updates and concat", {
  m_sum <- MeasureMonoid(function(a, b) a + b, 0, function(el) el)
  m_size <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  ms <- list(sum = m_sum, .size = m_size)

  t1 <- tree_from(1:3, monoid = ms)
  t1 <- append(t1, 4)
  t2 <- tree_from(10:11, monoid = ms)
  t <- concat_trees(t1, t2)

  root_measures <- attr(t, "measures")
  testthat::expect_identical(root_measures$sum, 31)
  testthat::expect_identical(root_measures$.size, 6)
})
