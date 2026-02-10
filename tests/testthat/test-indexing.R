testthat::test_that("read indexing supports [] and [[ with positive integer indices", {
  r <- MeasureMonoid(function(a, b) a + b, 0, function(el) el)
  t <- tree_from(1:6, monoid = r)

  testthat::expect_equal(t[[3]], 3)

  s <- t[c(2, 4, 6)]
  testthat::expect_identical(reduce_left(s), 12)
  testthat::expect_identical(attr(s, "measure"), 12)
})

testthat::test_that("indexing enforces bounds and integer-only indices", {
  r <- MeasureMonoid(function(a, b) a + b, 0, function(el) el)
  t <- tree_from(1:4, monoid = r)

  testthat::expect_error(t[[0]], "positive integer")
  testthat::expect_error(t[[5]], "out of bounds")
  testthat::expect_error(t[[c(1, 2)]], "\\[\\[ expects exactly one index")
  testthat::expect_error(t[c(1, -2)], "positive integer")
  testthat::expect_error(t[c(1, 2.5)], "integer indices")
})

testthat::test_that("replacement indexing supports [[<- and [<- with exact-size semantics", {
  r <- MeasureMonoid(function(a, b) a + b, 0, function(el) el)
  t <- tree_from(1:5, monoid = r)

  t2 <- t
  t2[[2]] <- 20
  testthat::expect_identical(reduce_left(t2), 33)
  testthat::expect_identical(t2[[2]], 20)

  t3 <- t
  t3[c(1, 5)] <- list(10, 50)
  testthat::expect_identical(reduce_left(t3), 69)
  testthat::expect_identical(t3[[1]], 10)
  testthat::expect_identical(t3[[5]], 50)

  testthat::expect_error({
    t4 <- t
    t4[c(1, 2)] <- list(99)
  }, "Replacement length must match index length exactly")
})

testthat::test_that("size measure is available by default for indexing even if not provided", {
  sum_only <- MeasureMonoid(function(a, b) a + b, 0, function(el) el)
  t <- tree_from(1:3, monoid = sum_only)
  ms <- attr(t, "measures")
  testthat::expect_identical(ms$.size, 3)
  testthat::expect_equal(t[[2]], 2)
})
