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

testthat::test_that("monoid specs are normalized to canonical named layout", {
  raw <- list(
    function(a, b) a + b,
    0L,
    function(el) as.integer(el)
  )
  class(raw) <- c("measure_monoid", "MeasureMonoid", "list")

  x <- as_flexseq(1:5, monoids = list(sum = raw))
  ms <- attr(x, "monoids", exact = TRUE)
  sum_m <- ms$sum

  testthat::expect_identical(names(sum_m), c("f", "i", "measure"))
  testthat::expect_true(is.function(sum_m$f))
  testthat::expect_true(is.function(sum_m$measure))
  testthat::expect_identical(sum_m$measure(7L), 7L)
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

testthat::test_that("peek/pop helpers work and are persistent", {
  x <- as_flexseq(letters[1:4])

  testthat::expect_identical(peek_front(x), "a")
  testthat::expect_identical(peek_back(x), "d")

  pf <- pop_front(x)
  testthat::expect_identical(pf$element, "a")
  testthat::expect_identical(as.list(pf$rest), as.list(letters[2:4]))

  pb <- pop_back(x)
  testthat::expect_identical(pb$element, "d")
  testthat::expect_identical(as.list(pb$rest), as.list(letters[1:3]))

  testthat::expect_identical(as.list(x), as.list(letters[1:4]))
})

testthat::test_that("peek/pop helpers validate empty input", {
  x <- flexseq()
  testthat::expect_error(peek_front(x), "empty sequence")
  testthat::expect_error(peek_back(x), "empty sequence")
  testthat::expect_error(pop_front(x), "empty sequence")
  testthat::expect_error(pop_back(x), "empty sequence")
})
