testthat::test_that("fapply dispatches for flexseq", {
  x <- as_flexseq(1:4)
  y <- fapply(x, function(v) v * 3)

  testthat::expect_s3_class(y, "flexseq")
  testthat::expect_identical(lapply(.ft_to_list(y), .ft_strip_name), as.list(c(3, 6, 9, 12)))
  testthat::expect_error(fapply(x, 1), "`FUN` must be a function")
})

testthat::test_that("fapply dispatches for priority_queue", {
  q <- priority_queue("a", "bb", "ccc", priorities = c(1, 3, 2))
  q2 <- fapply(q, function(item, priority, name) {
    list(item = toupper(item), priority = priority + 2 * nchar(item))
  })

  testthat::expect_s3_class(q2, "priority_queue")
  testthat::expect_equal(peek_min(q2), "A")
  testthat::expect_equal(peek_max(q2), "CCC")
  testthat::expect_error(fapply(q, 1), "`FUN` must be a function")
})

testthat::test_that("fapply dispatches for ordered_sequence", {
  xs <- as_ordered_sequence(list("x1", "x2", "x3"), keys = c(1, 1, 2))

  xs_item <- fapply(xs, function(item, key, name) {
    list(item = toupper(item))
  })
  testthat::expect_s3_class(xs_item, "ordered_sequence")
  testthat::expect_equal(as.list(xs_item), list("X1", "X2", "X3"))

  xs_rekey <- fapply(xs, function(item, key, name) {
    list(key = if(key == 1) 3 else 1)
  })
  testthat::expect_equal(as.list(xs_rekey), list("x3", "x1", "x2"))
  testthat::expect_equal(peek_key(xs_rekey, 3), "x1")
  testthat::expect_error(fapply(xs, 1), "`FUN` must be a function")
})
