testthat::test_that("apply default preserves base behavior for arrays", {
  m <- matrix(1:6, nrow = 2)
  testthat::expect_equal(apply(m, 1, sum), base::apply(m, 1, sum))
})

testthat::test_that("apply dispatches for flexseq", {
  x <- as_flexseq(1:4)
  y <- apply(x, function(v) v * 3)

  testthat::expect_s3_class(y, "flexseq")
  testthat::expect_identical(lapply(.ft_to_list(y), .ft_strip_name), as.list(c(3, 6, 9, 12)))
  testthat::expect_error(apply(x, 1, sum), "MARGIN")
})

testthat::test_that("apply dispatches for priority_queue", {
  q <- priority_queue("a", "bb", "ccc", priorities = c(1, 3, 2))
  q2 <- apply(q, function(item, priority, seq_id, name) {
    list(item = toupper(item), priority = priority + 2 * nchar(item))
  })

  testthat::expect_s3_class(q2, "priority_queue")
  testthat::expect_equal(peek_min(q2), "A")
  testthat::expect_equal(peek_max(q2), "CCC")
  testthat::expect_error(apply(q, 1, sum), "MARGIN")
})

testthat::test_that("apply dispatches for ordered_multiset", {
  ms <- as_ordered_multiset(list("x1", "x2", "x3"), keys = c(1, 1, 2))

  ms_item <- apply(ms, function(item, key, seq_id, name) {
    list(item = toupper(item))
  })
  testthat::expect_s3_class(ms_item, "ordered_multiset")
  testthat::expect_equal(as.list(ms_item), list("X1", "X2", "X3"))

  ms_rekey <- apply(ms, function(item, key, seq_id, name) {
    list(key = if(key == 1) 3 else 1)
  })
  testthat::expect_equal(as.list(ms_rekey), list("x3", "x1", "x2"))
  testthat::expect_equal(peek_key(ms_rekey, 3), "x1")
  testthat::expect_error(apply(ms, 1, sum), "MARGIN")
})

testthat::test_that("apply ordered_multiset reset_ties updates next_seq only when requested", {
  ms <- as_ordered_multiset(list("a", "b", "c", "d"), keys = c(1, 2, 3, 4))
  ms <- delete_one(ms, 4)
  testthat::expect_identical(attr(ms, "oms_next_seq", exact = TRUE), 5)

  keep <- apply(ms, function(item, key, seq_id, name) list(), reset_ties = FALSE)
  reset <- apply(ms, function(item, key, seq_id, name) list(), reset_ties = TRUE)

  testthat::expect_identical(attr(keep, "oms_next_seq", exact = TRUE), 5)
  testthat::expect_identical(attr(reset, "oms_next_seq", exact = TRUE), 4)
})
