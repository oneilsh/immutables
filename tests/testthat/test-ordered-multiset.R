testthat::test_that("constructor sorts by key and preserves stable duplicate order", {
  ms <- as_ordered_multiset(
    list("bbb", "a", "cc", "dd", "e"),
    keys = c(3, 1, 2, 2, 1)
  )

  testthat::expect_s3_class(ms, "ordered_multiset")
  testthat::expect_equal(as.list(ms), list("a", "e", "cc", "dd", "bbb"))
  testthat::expect_identical(length(ms), 5L)
  testthat::expect_equal(attr(ms, "oms_next_seq", exact = TRUE), 6)
})

testthat::test_that("insert_ms appends after equal-key block and increments seq", {
  ms <- as_ordered_multiset(list("bb", "aa", "c"), keys = c(2, 2, 1))
  ms2 <- insert_ms(ms, "dd", key = 2)

  testthat::expect_equal(as.list(ms2), list("c", "bb", "aa", "dd"))
  testthat::expect_equal(attr(ms2, "oms_next_seq", exact = TRUE), attr(ms, "oms_next_seq", exact = TRUE) + 1)
})

testthat::test_that("lower_bound and upper_bound behave at boundaries", {
  ms <- as_ordered_multiset(list("bbb", "a", "cc", "dddd"), keys = c(3, 1, 2, 4))

  lb1 <- lower_bound(ms, 2)
  ub1 <- upper_bound(ms, 2)
  lb9 <- lower_bound(ms, 9)

  testthat::expect_true(lb1$found)
  testthat::expect_identical(lb1$index, 2L)
  testthat::expect_equal(lb1$element, "cc")

  testthat::expect_true(ub1$found)
  testthat::expect_identical(ub1$index, 3L)
  testthat::expect_equal(ub1$element, "bbb")

  testthat::expect_false(lb9$found)
  testthat::expect_null(lb9$index)
})

testthat::test_that("count_key and delete operations follow multiplicities", {
  ms <- as_ordered_multiset(list("aa", "bb", "c", "dd", "e"), keys = c(2, 2, 1, 2, 1))

  testthat::expect_identical(count_key(ms, 2), 3L)
  testthat::expect_identical(count_key(ms, 1), 2L)
  testthat::expect_identical(count_key(ms, 9), 0L)

  ms_one <- delete_one(ms, 2)
  testthat::expect_equal(as.list(ms_one), list("c", "e", "bb", "dd"))

  ms_all <- delete_all(ms, 2)
  testthat::expect_equal(as.list(ms_all), list("c", "e"))

  ms_absent <- delete_one(ms, 99)
  testthat::expect_equal(as.list(ms_absent), as.list(ms))
})

testthat::test_that("count_between and elements_between support inclusivity flags", {
  ms <- as_ordered_multiset(list("a", "bb", "cc", "ddd", "eeee"), keys = c(1, 2, 2, 3, 4))

  testthat::expect_identical(count_between(ms, 2, 3), 3L)
  testthat::expect_identical(count_between(ms, 2, 3, include_hi = FALSE), 2L)
  testthat::expect_identical(count_between(ms, 2, 3, include_lo = FALSE), 1L)
  testthat::expect_identical(count_between(ms, 9, 10), 0L)

  testthat::expect_equal(elements_between(ms, 2, 3), list("bb", "cc", "ddd"))
  testthat::expect_equal(elements_between(ms, 2, 3, include_hi = FALSE), list("bb", "cc"))
  testthat::expect_equal(elements_between(ms, 2, 3, include_lo = FALSE), list("ddd"))
})

testthat::test_that("union_ms uses bag max multiplicities with deterministic payload rules", {
  x <- as_ordered_multiset(list("aa", "bb", "c", "ddd"), keys = c(2, 2, 1, 3))
  y <- as_ordered_multiset(list("xx", "z", "qq", "rrrr"), keys = c(2, 1, 2, 4))

  u <- union_ms(x, y)

  # len 1: max(1,1)=1 from left ("c")
  # len 2: max(2,2)=2 keeps all left len2 entries
  # len 3: max(1,0)=1 from left
  # len 4: max(0,1)=1 from right
  testthat::expect_equal(as.list(u), list("c", "aa", "bb", "ddd", "rrrr"))
})

testthat::test_that("intersection_ms and difference_ms follow bag semantics", {
  x <- as_ordered_multiset(list("a", "bb", "cc", "ddd"), keys = c(1, 2, 2, 3))
  y <- as_ordered_multiset(list("z", "xx", "qq", "rrrr"), keys = c(1, 2, 2, 4))

  i <- intersection_ms(x, y)
  d <- difference_ms(x, y)

  # intersection keeps left payloads for common multiplicity min
  testthat::expect_equal(as.list(i), list("a", "bb", "cc"))
  # difference removes min-count from left groups
  testthat::expect_equal(as.list(d), list("ddd"))
})

testthat::test_that("set operations require compatible key types", {
  x <- as_ordered_multiset(list("aa", "b"), keys = c(2, 1))
  y <- as_ordered_multiset(list("aa", "b"), keys = c("2", "1"))

  testthat::expect_error(union_ms(x, y), "incompatible")
  testthat::expect_error(intersection_ms(x, y), "incompatible")
  testthat::expect_error(difference_ms(x, y), "incompatible")
})

testthat::test_that("ordered_multiset validates keys input", {
  testthat::expect_error(
    as_ordered_multiset(list(1, 2), keys = list(c(1, 1), 2)),
    "scalar"
  )
  testthat::expect_error(
    as_ordered_multiset(list(1), keys = list(NA_real_)),
    "non-missing"
  )
  testthat::expect_error(
    as_ordered_multiset(list(1), keys = list(list(1))),
    "numeric, character, or logical"
  )
  testthat::expect_error(
    as_ordered_multiset(list(1, 2), keys = 1),
    "length must match"
  )
  testthat::expect_error(
    as_ordered_multiset(list(1, 2)),
    "`keys` is required"
  )
})

testthat::test_that("set operations support merge engine toggle", {
  old_engine <- getOption("immutables.oms.merge_engine")
  on.exit({
    if(is.null(old_engine)) {
      options(immutables.oms.merge_engine = NULL)
    } else {
      options(immutables.oms.merge_engine = old_engine)
    }
  }, add = TRUE)

  x <- as_ordered_multiset(list("aa", "bb", "c", "ddd"), keys = c(2, 2, 1, 3))
  y <- as_ordered_multiset(list("xx", "z", "qq", "rrrr"), keys = c(2, 1, 2, 4))

  options(immutables.oms.merge_engine = "legacy_r")
  u_legacy <- union_ms(x, y)

  options(immutables.oms.merge_engine = "auto")
  u_auto <- union_ms(x, y)

  testthat::expect_equal(as.list(u_auto), as.list(u_legacy))
})

testthat::test_that("merge engine option validates values", {
  old_engine <- getOption("immutables.oms.merge_engine")
  on.exit({
    if(is.null(old_engine)) {
      options(immutables.oms.merge_engine = NULL)
    } else {
      options(immutables.oms.merge_engine = old_engine)
    }
  }, add = TRUE)

  x <- as_ordered_multiset(list("aa", "bb"), keys = c(2, 2))
  y <- as_ordered_multiset(list("cc", "d"), keys = c(2, 1))
  options(immutables.oms.merge_engine = "bogus")
  testthat::expect_error(union_ms(x, y), "merge_engine")
})

testthat::test_that("insert_ms requires an explicit key", {
  ms <- as_ordered_multiset(list("a"), keys = 1)
  testthat::expect_error(insert_ms(ms, "b"), "argument \"key\" is missing")
})
