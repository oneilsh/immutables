testthat::test_that("str() works for flexseq-like classes with restricted indexing", {
  x <- as_flexseq(list(a = 1, b = 2, c = 3))
  q <- priority_queue(one = 1, two = 2, three = 3, priorities = 1:3)
  os <- ordered_sequence(one = 1, two = 2, three = 3, keys = 1:3)
  ix <- interval_index(
    one = 1, two = 2, three = 3,
    start = c(1, 5, 10),
    end = c(3, 7, 12)
  )

  out_x <- testthat::expect_no_error(utils::capture.output(utils::str(x)))
  out_q <- testthat::expect_no_error(utils::capture.output(utils::str(q)))
  out_os <- testthat::expect_no_error(utils::capture.output(utils::str(os)))
  out_ix <- testthat::expect_no_error(utils::capture.output(utils::str(ix)))

  testthat::expect_true(any(grepl("^List of ", out_x)))
  testthat::expect_true(any(grepl("^List of ", out_q)))
  testthat::expect_true(any(grepl("^List of ", out_os)))
  testthat::expect_true(any(grepl("^List of ", out_ix)))

  blocked_msg <- "supports scalar character names only"
  testthat::expect_false(any(grepl(blocked_msg, out_q, fixed = TRUE)))
  testthat::expect_false(any(grepl(blocked_msg, out_os, fixed = TRUE)))
  testthat::expect_false(any(grepl(blocked_msg, out_ix, fixed = TRUE)))
})
