testthat::test_that("fapply maps flexseq values in order", {
  x <- as_flexseq(1:5)
  y <- fapply(x, function(v) v * 10)

  testthat::expect_s3_class(y, "flexseq")
  testthat::expect_identical(as.integer(node_measure(y, ".size")), 5L)
  testthat::expect_identical(
    lapply(.ft_to_list(y), .ft_strip_name),
    as.list(c(10, 20, 30, 40, 50))
  )
})

testthat::test_that("fapply preserves element names for named flexseq", {
  x <- as_flexseq(setNames(as.list(1:4), c("a", "b", "c", "d")))
  y <- fapply(x, function(v) v + 1L)

  nms <- .ft_collect_names(y)
  testthat::expect_identical(nms, c("a", "b", "c", "d"))
  testthat::expect_identical(y[["c"]], 4L)
})

testthat::test_that("fapply preserves user monoids for flexseq", {
  sum_m <- measure_monoid(`+`, 0, as.numeric)
  x <- as_flexseq(1:4, monoids = list(sum = sum_m))
  y <- fapply(x, function(v) v * 2)

  ms <- attr(y, "monoids", exact = TRUE)
  testthat::expect_true(!is.null(ms[[".size"]]))
  testthat::expect_true(!is.null(ms[[".named_count"]]))
  testthat::expect_true(!is.null(ms[["sum"]]))
  testthat::expect_identical(node_measure(y, "sum"), 20)
  testthat::expect_identical(as.integer(node_measure(y, ".size")), 4L)
})

testthat::test_that("fapply recomputes preserved user monoids for flexseq", {
  sum_m <- measure_monoid(`+`, 0, as.numeric)
  x <- as_flexseq(1:4, monoids = list(sum = sum_m))
  y <- fapply(x, function(v) v * 2)

  testthat::expect_true(!is.null(attr(y, "monoids", exact = TRUE)[["sum"]]))
  testthat::expect_identical(node_measure(y, "sum"), 20)
  testthat::expect_identical(as.integer(node_measure(y, ".size")), 4L)
})

testthat::test_that("fapply validates flexseq inputs", {
  x <- as_flexseq(1:3)
  testthat::expect_error(fapply.flexseq(list(1, 2, 3), FUN = identity), "`x` must be a flexseq")
  testthat::expect_error(fapply(x, 1), "`FUN` must be a function")
})
