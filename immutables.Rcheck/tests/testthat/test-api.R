testthat::test_that("reduce uses explicit monoid object", {
  r <- measure_monoid(function(a, b) a + b, 0, function(el) 0)
  t <- as_flexseq(1:3)
  testthat::expect_identical(fold_left(t, r), 6)
  testthat::expect_identical(fold_right(t, r), 6)
})

testthat::test_that("tree construction defaults to .size monoid", {
  t <- as_flexseq(1:3)
  ms <- attr(t, "measures")
  testthat::expect_identical(ms$.size, 3)
  testthat::expect_true(!is.null(attr(t, "monoids")[[".size"]]))

  e <- flexseq()
  testthat::expect_true(e %isa% Empty)
  testthat::expect_identical(attr(e, "measures")$.size, 0)
})

testthat::test_that("flexseq class does not inherit from list and length is element count", {
  t <- as_flexseq(letters[1:5])
  testthat::expect_true("flexseq" %in% class(t))
  testthat::expect_false("list" %in% class(t))
  testthat::expect_identical(length(t), 5L)
})

testthat::test_that("add_monoids merges and supports overwrite flag", {
  t <- as_flexseq(1:5)
  sum_m <- measure_monoid(function(a, b) a + b, 0, function(el) el)
  t2 <- add_monoids(t, list(sum = sum_m))

  testthat::expect_identical(attr(t2, "measures")$sum, 15)
  testthat::expect_identical(attr(t2, "measures")$.size, 5)

  sum2 <- measure_monoid(function(a, b) a + b + 1, 0, function(el) el)
  testthat::expect_error(
    add_monoids(t2, list(sum = sum2), overwrite = FALSE),
    "already exist"
  )

  t3 <- add_monoids(t2, list(sum = sum2), overwrite = TRUE)
  testthat::expect_true(!identical(attr(t2, "measures")$sum, attr(t3, "measures")$sum))
})

testthat::test_that("concat_trees unions monoids on shared names", {
  a <- measure_monoid(function(x, y) x + y, 0, function(el) el)
  b <- measure_monoid(function(x, y) x + y, 0, function(el) 1)

  t1 <- as_flexseq(1:2, monoids = list(sum = a))
  t2 <- as_flexseq(3:4, monoids = list(cnt = b))
  t <- c(t1, t2)
  testthat::expect_true(all(c(".size", "sum", "cnt") %in% names(attr(t, "monoids"))))

  # shared name path: left definition is assumed authoritative
  t3 <- as_flexseq(1:2, monoids = list(sum = a))
  t4 <- as_flexseq(3:4, monoids = list(sum = measure_monoid(function(x, y) x + y, 0, function(el) 1)))
  t_merged <- c(t3, t4)
  testthat::expect_true("sum" %in% names(attr(t_merged, "monoids")))
})
