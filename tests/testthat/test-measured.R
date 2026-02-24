testthat::test_that("monoid caches are stored in measures attr", {
  m_count <- measure_monoid(function(a, b) a + b, 0, function(el) 1)
  t <- add_monoids(as_flexseq(letters[1:5]), list(count = m_count))

  ms <- attr(t, "measures")
  testthat::expect_identical(ms$count, 5)
  testthat::expect_identical(ms$.size, 5)
})

testthat::test_that("measures propagate through prepend/append", {
  m_count <- measure_monoid(function(a, b) a + b, 0, function(el) 1)
  t <- add_monoids(flexseq(), list(count = m_count))
  t <- push_back(t, "a")
  t <- push_back(t, "b")
  t <- push_front(t, "z")

  ms <- attr(t, "measures")
  testthat::expect_identical(ms$count, 3)
  testthat::expect_identical(ms$.size, 3)
})

testthat::test_that("custom measure works with list elements", {
  m_len <- measure_monoid(function(a, b) a + b, 0, function(el) length(el))
  t <- add_monoids(flexseq(), list(len = m_len))
  t <- push_back(t, list(1, 2, 3))
  t <- push_back(t, list("a"))
  t <- push_back(t, list(TRUE, FALSE))

  testthat::expect_identical(attr(t, "measures")$len, 6)
})

testthat::test_that("multiple measures propagate through concat", {
  m_sum <- measure_monoid(function(a, b) a + b, 0, function(el) el)
  m_count <- measure_monoid(function(a, b) a + b, 0, function(el) 1)
  ms <- list(sum = m_sum, count = m_count)

  t1 <- add_monoids(as_flexseq(1:3), ms)
  t1 <- push_back(t1, 4)
  t2 <- add_monoids(as_flexseq(10:11), ms)
  t <- c(t1, t2)

  root <- attr(t, "measures")
  testthat::expect_identical(root$sum, 31)
  testthat::expect_identical(root$count, 6)
  testthat::expect_identical(root$.size, 6)
})
