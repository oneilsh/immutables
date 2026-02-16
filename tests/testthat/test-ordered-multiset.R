testthat::test_that("constructor sorts by key and preserves stable duplicate order", {
  ms <- as_ordered_multiset(
    list("bbb", "a", "cc", "dd", "e"),
    key = nchar
  )

  testthat::expect_s3_class(ms, "ordered_multiset")
  testthat::expect_equal(as.list(ms), list("a", "e", "cc", "dd", "bbb"))
  testthat::expect_identical(length(ms), 5L)
  testthat::expect_equal(attr(ms, "oms_next_seq", exact = TRUE), 6)
})

testthat::test_that("insert_ms appends after equal-key block and increments seq", {
  ms <- as_ordered_multiset(list("bb", "aa", "c"), key = nchar)
  ms2 <- insert_ms(ms, "dd")

  testthat::expect_equal(as.list(ms2), list("c", "bb", "aa", "dd"))
  testthat::expect_equal(attr(ms2, "oms_next_seq", exact = TRUE), attr(ms, "oms_next_seq", exact = TRUE) + 1)
})

testthat::test_that("lower_bound and upper_bound behave at boundaries", {
  ms <- as_ordered_multiset(list("bbb", "a", "cc", "dddd"), key = nchar)

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
  ms <- as_ordered_multiset(list("aa", "bb", "c", "dd", "e"), key = nchar)

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
  ms <- as_ordered_multiset(list("a", "bb", "cc", "ddd", "eeee"), key = nchar)

  testthat::expect_identical(count_between(ms, 2, 3), 3L)
  testthat::expect_identical(count_between(ms, 2, 3, include_hi = FALSE), 2L)
  testthat::expect_identical(count_between(ms, 2, 3, include_lo = FALSE), 1L)
  testthat::expect_identical(count_between(ms, 9, 10), 0L)

  testthat::expect_equal(elements_between(ms, 2, 3), list("bb", "cc", "ddd"))
  testthat::expect_equal(elements_between(ms, 2, 3, include_hi = FALSE), list("bb", "cc"))
  testthat::expect_equal(elements_between(ms, 2, 3, include_lo = FALSE), list("ddd"))
})

testthat::test_that("union_ms uses bag max multiplicities with deterministic payload rules", {
  key_fn <- nchar
  x <- as_ordered_multiset(list("aa", "bb", "c", "ddd"), key = key_fn)
  y <- as_ordered_multiset(list("xx", "z", "qq", "rrrr"), key = key_fn)

  u <- union_ms(x, y)

  # len 1: max(1,1)=1 from left ("c")
  # len 2: max(2,2)=2 keeps all left len2 entries
  # len 3: max(1,0)=1 from left
  # len 4: max(0,1)=1 from right
  testthat::expect_equal(as.list(u), list("c", "aa", "bb", "ddd", "rrrr"))
})

testthat::test_that("intersection_ms and difference_ms follow bag semantics", {
  key_fn <- nchar
  x <- as_ordered_multiset(list("a", "bb", "cc", "ddd"), key = key_fn)
  y <- as_ordered_multiset(list("z", "xx", "qq", "rrrr"), key = key_fn)

  i <- intersection_ms(x, y)
  d <- difference_ms(x, y)

  # intersection keeps left payloads for common multiplicity min
  testthat::expect_equal(as.list(i), list("a", "bb", "cc"))
  # difference removes min-count from left groups
  testthat::expect_equal(as.list(d), list("ddd"))
})

testthat::test_that("set operations require compatible key extractors", {
  x <- as_ordered_multiset(list("aa", "b"), key = nchar)
  y <- as_ordered_multiset(list("aa", "b"), key = identity)

  testthat::expect_error(union_ms(x, y), "incompatible")
  testthat::expect_error(intersection_ms(x, y), "incompatible")
  testthat::expect_error(difference_ms(x, y), "incompatible")
})

testthat::test_that("ordered_multiset validates key outputs", {
  testthat::expect_error(
    as_ordered_multiset(list(1, 2), key = function(x) c(x, x)),
    "scalar"
  )
  testthat::expect_error(
    as_ordered_multiset(list(1), key = function(x) NA_real_),
    "non-missing"
  )
  testthat::expect_error(
    as_ordered_multiset(list(1), key = function(x) list(1)),
    "numeric, character, or logical"
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

  x <- as_ordered_multiset(list("aa", "bb", "c", "ddd"), key = nchar)
  y <- as_ordered_multiset(list("xx", "z", "qq", "rrrr"), key = nchar)

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

  x <- as_ordered_multiset(list("aa", "bb"), key = nchar)
  y <- as_ordered_multiset(list("cc", "d"), key = nchar)
  options(immutables.oms.merge_engine = "bogus")
  testthat::expect_error(union_ms(x, y), "merge_engine")
})
