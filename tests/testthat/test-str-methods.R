testthat::test_that("str() works for flexseq-like classes with restricted indexing", {
  x <- as_flexseq(list(a = 1, b = 2, c = 3))
  q <- priority_queue(one = 1, two = 2, three = 3, priorities = 1:3)
  os <- ordered_sequence(one = 1, two = 2, three = 3, keys = 1:3)
  ix <- interval_index(
    one = 1, two = 2, three = 3,
    start = c(1, 5, 10),
    end = c(3, 7, 12)
  )

  testthat::expect_no_error(utils::capture.output(utils::str(x)))
  testthat::expect_no_error(utils::capture.output(utils::str(q)))
  testthat::expect_no_error(utils::capture.output(utils::str(os)))
  testthat::expect_no_error(utils::capture.output(utils::str(ix)))
})
