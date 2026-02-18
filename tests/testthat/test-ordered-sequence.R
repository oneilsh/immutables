testthat::test_that("constructor sorts by key and preserves stable duplicate order", {
  xs <- as_ordered_sequence(
    list("bbb", "a", "cc", "dd", "e"),
    keys = c(3, 1, 2, 2, 1)
  )

  testthat::expect_s3_class(xs, "ordered_sequence")
  testthat::expect_equal(as.list(xs), list("a", "e", "cc", "dd", "bbb"))
  testthat::expect_identical(length(xs), 5L)
})

testthat::test_that("insert appends at right edge of equal-key block (FIFO ties)", {
  xs <- as_ordered_sequence(list("bb", "aa", "c"), keys = c(2, 2, 1))
  xs2 <- insert(xs, "dd", key = 2)

  testthat::expect_equal(as.list(xs2), list("c", "bb", "aa", "dd"))
  testthat::expect_equal(peek_key(xs2, 2), "bb")

  out1 <- pop_key(xs2, 2)
  out2 <- pop_key(out1$sequence, 2)
  testthat::expect_equal(out1$element, "bb")
  testthat::expect_equal(out2$element, "aa")
})

testthat::test_that("lower_bound and upper_bound behave at boundaries", {
  xs <- as_ordered_sequence(list("bbb", "a", "cc", "dddd"), keys = c(3, 1, 2, 4))

  lb1 <- lower_bound(xs, 2)
  ub1 <- upper_bound(xs, 2)
  lb9 <- lower_bound(xs, 9)

  testthat::expect_true(lb1$found)
  testthat::expect_identical(lb1$index, 2L)
  testthat::expect_equal(lb1$element, "cc")

  testthat::expect_true(ub1$found)
  testthat::expect_identical(ub1$index, 3L)
  testthat::expect_equal(ub1$element, "bbb")

  testthat::expect_false(lb9$found)
  testthat::expect_null(lb9$index)
})

testthat::test_that("pop_key supports first/all removal by key span", {
  xs <- as_ordered_sequence(list("aa", "bb", "c", "dd", "e"), keys = c(2, 2, 1, 2, 1))

  one <- pop_key(xs, 2)
  testthat::expect_equal(one$element, "aa")
  testthat::expect_equal(as.list(one$sequence), list("c", "e", "bb", "dd"))

  all <- pop_key(xs, 2, which = "all")
  testthat::expect_s3_class(all$element, "ordered_sequence")
  testthat::expect_equal(as.list(all$element), list("aa", "bb", "dd"))
  testthat::expect_equal(as.list(all$sequence), list("c", "e"))

  miss_one <- pop_key(xs, 99)
  testthat::expect_null(miss_one$element)
  testthat::expect_equal(as.list(miss_one$sequence), as.list(xs))

  miss_all <- pop_key(xs, 99, which = "all")
  testthat::expect_s3_class(miss_all$element, "ordered_sequence")
  testthat::expect_identical(length(miss_all$element), 0L)
  testthat::expect_equal(as.list(miss_all$sequence), as.list(xs))
})

testthat::test_that("elements_between supports inclusivity flags", {
  xs <- as_ordered_sequence(list("a", "bb", "cc", "ddd", "eeee"), keys = c(1, 2, 2, 3, 4))

  e_closed <- elements_between(xs, 2, 3)
  e_open_hi <- elements_between(xs, 2, 3, include_to = FALSE)
  e_open_lo <- elements_between(xs, 2, 3, include_from = FALSE)
  e_miss <- elements_between(xs, 9, 10)

  testthat::expect_equal(e_closed, list("bb", "cc", "ddd"))
  testthat::expect_equal(e_open_hi, list("bb", "cc"))
  testthat::expect_equal(e_open_lo, list("ddd"))
  testthat::expect_equal(e_miss, list())
})

testthat::test_that("peek_key and pop_key are stable within duplicate key blocks", {
  xs <- as_ordered_sequence(list("a1", "b1", "a2", "a3"), keys = c(1, 2, 1, 1))

  testthat::expect_equal(peek_key(xs, 1), "a1")
  peek_all <- peek_key(xs, 1, which = "all")
  testthat::expect_s3_class(peek_all, "ordered_sequence")
  testthat::expect_equal(as.list(peek_all), list("a1", "a2", "a3"))
  out <- pop_key(xs, 1)
  testthat::expect_equal(out$element, "a1")
  testthat::expect_equal(out$key, 1)
  testthat::expect_equal(as.list(out$sequence), list("a2", "a3", "b1"))

  testthat::expect_null(peek_key(xs, 9))
  peek_miss_all <- peek_key(xs, 9, which = "all")
  testthat::expect_s3_class(peek_miss_all, "ordered_sequence")
  testthat::expect_identical(length(peek_miss_all), 0L)
  testthat::expect_identical(peek_key(xs, 9, if_missing = NA_character_), NA_character_)
  miss <- pop_key(xs, 9)
  testthat::expect_null(miss$element)
  testthat::expect_null(miss$key)
  testthat::expect_equal(as.list(miss$sequence), as.list(xs))
})

testthat::test_that("count helpers match range and key multiplicities", {
  xs <- as_ordered_sequence(list("a", "bb", "cc", "ddd", "eeee"), keys = c(1, 2, 2, 3, 4))

  testthat::expect_identical(count_key(xs, 2), 2L)
  testthat::expect_identical(count_key(xs, 9), 0L)

  testthat::expect_identical(count_between(xs, 2, 3), 3L)
  testthat::expect_identical(count_between(xs, 2, 3, include_to = FALSE), 2L)
  testthat::expect_identical(count_between(xs, 2, 3, include_from = FALSE), 1L)
  testthat::expect_identical(count_between(xs, 9, 10), 0L)
})

testthat::test_that("order-breaking writes are blocked on ordered types", {
  xs <- as_ordered_sequence(list("a", "b"), keys = c(1, 2))

  testthat::expect_error(c(xs, xs), "not supported")
  testthat::expect_error(push_back(xs, "c"), "not supported")
  testthat::expect_error(push_front(xs, "z"), "not supported")

  testthat::expect_error({ xs[[1]] <- "z" }, "not supported")
  testthat::expect_error({ xs[1] <- list("z") }, "not supported")
  testthat::expect_error({ xs$a <- "z" }, "not supported")
})

testthat::test_that("ordered subsetting requires strictly increasing mapped positions", {
  xs <- as_ordered_sequence(
    list(a = "xa", b = "xb", c = "xc", d = "xd"),
    keys = c(1, 2, 3, 4)
  )

  inc <- xs[c(1, 3)]
  testthat::expect_s3_class(inc, "ordered_sequence")
  testthat::expect_equal(as.list(inc), list("xa", "xc"))

  by_name <- xs[c("a", "c")]
  testthat::expect_equal(as.list(by_name), list("xa", "xc"))

  lgl <- xs[c(TRUE, FALSE)]
  testthat::expect_equal(as.list(lgl), list("xa", "xc"))

  testthat::expect_error(xs[c(3, 1)], "strictly increasing")
  testthat::expect_error(xs[c(2, 2)], "strictly increasing")
  testthat::expect_error(xs[c("c", "a")], "strictly increasing")
  testthat::expect_error(xs[c("a", "missing")], "Unknown element name")
})

testthat::test_that("pop helpers preserve ordered class", {
  xs <- as_ordered_sequence(list("x1", "x2", "x3"), keys = c(1, 2, 3))
  pf <- pop_front(xs)
  pb <- pop_back(xs)

  testthat::expect_identical(pf$element, "x1")
  testthat::expect_s3_class(pf$rest, "ordered_sequence")
  testthat::expect_equal(as.list(pf$rest), list("x2", "x3"))

  testthat::expect_identical(pb$element, "x3")
  testthat::expect_s3_class(pb$rest, "ordered_sequence")
  testthat::expect_equal(as.list(pb$rest), list("x1", "x2"))
})

testthat::test_that("apply dispatches for ordered_sequence and no reset_ties arg", {
  xs <- as_ordered_sequence(list("x1", "x2", "x3"), keys = c(1, 1, 2))

  xs_item <- apply(xs, function(item, key, name) {
    list(item = toupper(item))
  })
  testthat::expect_s3_class(xs_item, "ordered_sequence")
  testthat::expect_equal(as.list(xs_item), list("X1", "X2", "X3"))

  xs_rekey <- apply(xs, function(item, key, name) {
    list(key = if(key == 1) 3 else 1)
  })
  testthat::expect_equal(as.list(xs_rekey), list("x3", "x1", "x2"))
  testthat::expect_equal(peek_key(xs_rekey, 3), "x1")
  testthat::expect_error(apply(xs, 1, sum), "MARGIN")
  testthat::expect_error(apply(xs, function(item, key, name) list(), reset_ties = TRUE), "unused")
})
