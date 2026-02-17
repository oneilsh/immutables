testthat::test_that("constructor sorts by key and preserves stable duplicate order", {
  xs <- as_ordered_sequence(
    list("bbb", "a", "cc", "dd", "e"),
    keys = c(3, 1, 2, 2, 1)
  )

  testthat::expect_s3_class(xs, "ordered_sequence")
  testthat::expect_false(inherits(xs, "ordered_multiset"))
  testthat::expect_equal(as.list(xs), list("a", "e", "cc", "dd", "bbb"))
  testthat::expect_identical(length(xs), 5L)
})

testthat::test_that("insert appends at right edge of equal-key block (FIFO ties)", {
  xs <- as_ordered_sequence(list("bb", "aa", "c"), keys = c(2, 2, 1))
  xs2 <- insert(xs, "dd", key = 2)

  testthat::expect_equal(as.list(xs2), list("c", "bb", "aa", "dd"))
  testthat::expect_equal(peek_key(xs2, 2), "bb")

  out1 <- extract_key(xs2, 2)
  out2 <- extract_key(out1$sequence, 2)
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

testthat::test_that("delete operations follow key spans", {
  xs <- as_ordered_sequence(list("aa", "bb", "c", "dd", "e"), keys = c(2, 2, 1, 2, 1))

  xs_one <- delete_one(xs, 2)
  testthat::expect_equal(as.list(xs_one), list("c", "e", "bb", "dd"))

  xs_all <- delete_all(xs, 2)
  testthat::expect_equal(as.list(xs_all), list("c", "e"))

  xs_absent <- delete_one(xs, 99)
  testthat::expect_equal(as.list(xs_absent), as.list(xs))
})

testthat::test_that("elements_between supports inclusivity flags", {
  xs <- as_ordered_sequence(list("a", "bb", "cc", "ddd", "eeee"), keys = c(1, 2, 2, 3, 4))

  e_closed <- elements_between(xs, 2, 3)
  e_open_hi <- elements_between(xs, 2, 3, include_hi = FALSE)
  e_open_lo <- elements_between(xs, 2, 3, include_lo = FALSE)
  e_miss <- elements_between(xs, 9, 10)

  testthat::expect_equal(e_closed, list("bb", "cc", "ddd"))
  testthat::expect_equal(e_open_hi, list("bb", "cc"))
  testthat::expect_equal(e_open_lo, list("ddd"))
  testthat::expect_equal(e_miss, list())
})

testthat::test_that("peek_key and extract_key are stable within duplicate key blocks", {
  xs <- as_ordered_sequence(list("a1", "b1", "a2", "a3"), keys = c(1, 2, 1, 1))

  testthat::expect_equal(peek_key(xs, 1), "a1")
  out <- extract_key(xs, 1)
  testthat::expect_equal(out$element, "a1")
  testthat::expect_equal(out$key, 1)
  testthat::expect_equal(as.list(out$sequence), list("a2", "a3", "b1"))

  testthat::expect_error(peek_key(xs, 9), "not found")
  testthat::expect_error(extract_key(xs, 9), "not found")
})

testthat::test_that("merge is additive, stable, and variadic", {
  x <- as_ordered_sequence(list("x2a", "x2b", "x1"), keys = c(2, 2, 1))
  y <- as_ordered_sequence(list("y2", "y1"), keys = c(2, 1))
  z <- as_ordered_sequence(list("z2", "z3"), keys = c(2, 3))

  mxy <- merge(x, y)
  testthat::expect_s3_class(mxy, "ordered_sequence")
  testthat::expect_equal(as.list(mxy), list("x1", "y1", "x2a", "x2b", "y2"))

  mxyz <- merge(x, y, z)
  testthat::expect_equal(as.list(mxyz), list("x1", "y1", "x2a", "x2b", "y2", "z2", "z3"))
})

testthat::test_that("merge rejects mixed ordered concrete classes", {
  xs <- as_ordered_sequence(list("a"), keys = 1)
  ms <- as_ordered_multiset(list("b"), keys = 1)
  testthat::expect_error(merge(xs, ms), "cannot mix")
  testthat::expect_error(merge(ms, xs), "cannot mix")
})

testthat::test_that("ordered_sequence set ops error with guidance", {
  x <- as_ordered_sequence(list("a"), keys = 1)
  y <- as_ordered_sequence(list("b"), keys = 1)
  testthat::expect_error(union(x, y), "ordered_multiset")
  testthat::expect_error(intersect(x, y), "ordered_multiset")
  testthat::expect_error(setdiff(x, y), "ordered_multiset")
})

testthat::test_that("order-breaking writes are blocked on ordered types", {
  xs <- as_ordered_sequence(list("a", "b"), keys = c(1, 2))
  ms <- as_ordered_multiset(list("a", "b"), keys = c(1, 2))

  testthat::expect_error(c(xs, xs), "merge")
  testthat::expect_error(append(xs, "c"), "not supported")
  testthat::expect_error(prepend(xs, "z"), "not supported")

  testthat::expect_error(c(ms, ms), "merge")
  testthat::expect_error(append(ms, "c"), "not supported")
  testthat::expect_error(prepend(ms, "z"), "not supported")

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
