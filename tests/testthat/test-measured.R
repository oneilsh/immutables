testthat::test_that("monoid caches are stored in measures attr", {
  m_count <- measure_monoid(function(a, b) a + b, 0, function(el) 1)
  t <- as_flexseq(letters[1:5], monoids = list(count = m_count))

  ms <- attr(t, "measures")
  testthat::expect_identical(ms$count, 5)
  testthat::expect_identical(ms$.size, 5)
})

testthat::test_that("measures propagate through prepend/append", {
  m_count <- measure_monoid(function(a, b) a + b, 0, function(el) 1)
  t <- flexseq(monoids = list(count = m_count))
  t <- append(t, "a")
  t <- append(t, "b")
  t <- prepend(t, "z")

  ms <- attr(t, "measures")
  testthat::expect_identical(ms$count, 3)
  testthat::expect_identical(ms$.size, 3)
})

testthat::test_that("custom measure works with list elements", {
  m_len <- measure_monoid(function(a, b) a + b, 0, function(el) length(el))
  t <- flexseq(monoids = list(len = m_len))
  t <- append(t, list(1, 2, 3))
  t <- append(t, list("a"))
  t <- append(t, list(TRUE, FALSE))

  testthat::expect_identical(attr(t, "measures")$len, 6)
})

testthat::test_that("multiple measures propagate through concat", {
  m_sum <- measure_monoid(function(a, b) a + b, 0, function(el) el)
  m_count <- measure_monoid(function(a, b) a + b, 0, function(el) 1)
  ms <- list(sum = m_sum, count = m_count)

  t1 <- as_flexseq(1:3, monoids = ms)
  t1 <- append(t1, 4)
  t2 <- as_flexseq(10:11, monoids = ms)
  testthat::expect_warning(
    t <- c(t1, t2),
    class = "immutables_monoid_assumption_warning"
  )

  root <- attr(t, "measures")
  testthat::expect_identical(root$sum, 31)
  testthat::expect_identical(root$count, 6)
  testthat::expect_identical(root$.size, 6)
})
