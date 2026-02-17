testthat::test_that("ordered_multiset constructors produce ordered_multiset subclass", {
  ms <- as_ordered_multiset(list("bbb", "a", "cc", "dd", "e"), keys = c(3, 1, 2, 2, 1))

  testthat::expect_s3_class(ms, "ordered_multiset")
  testthat::expect_s3_class(ms, "ordered_sequence")
  testthat::expect_equal(as.list(ms), list("a", "e", "cc", "dd", "bbb"))
})

testthat::test_that("ordered_multiset inherits ordered_sequence operations", {
  ms <- as_ordered_multiset(list("bb", "aa", "c"), keys = c(2, 2, 1))
  ms2 <- insert(ms, "dd", key = 2)

  testthat::expect_s3_class(ms2, "ordered_multiset")
  testthat::expect_equal(as.list(ms2), list("c", "bb", "aa", "dd"))
  testthat::expect_equal(peek_key(ms2, 2), "bb")
})

testthat::test_that("union/intersect/setdiff methods use ordered_multiset bag semantics", {
  x <- as_ordered_multiset(list("aa", "bb", "c", "ddd"), keys = c(2, 2, 1, 3))
  y <- as_ordered_multiset(list("xx", "z", "qq", "rrrr"), keys = c(2, 1, 2, 4))

  testthat::expect_equal(as.list(union(x, y)), list("c", "aa", "bb", "ddd", "rrrr"))
  testthat::expect_equal(as.list(intersect(x, y)), list("c", "aa", "bb"))
  testthat::expect_equal(as.list(setdiff(x, y)), list("ddd"))

  testthat::expect_equal(sort(union(c(1, 2, 2), c(2, 3))), c(1, 2, 3))
  testthat::expect_equal(intersect(c(1, 2, 2), c(2, 3)), 2)
  testthat::expect_equal(setdiff(c(1, 2, 2), c(2, 3)), 1)
})

testthat::test_that("set operations require compatible key types", {
  x <- as_ordered_multiset(list("aa", "b"), keys = c(2, 1))
  y <- as_ordered_multiset(list("aa", "b"), keys = c("2", "1"))

  testthat::expect_error(union(x, y), "incompatible")
  testthat::expect_error(intersect(x, y), "incompatible")
  testthat::expect_error(setdiff(x, y), "incompatible")
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
  u_legacy <- union(x, y)

  options(immutables.oms.merge_engine = "auto")
  u_auto <- union(x, y)

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
  testthat::expect_error(union(x, y), "merge_engine")
})


testthat::test_that("insert requires an explicit key", {
  ms <- as_ordered_multiset(list("a"), keys = 1)
  testthat::expect_error(insert(ms, "b"), "argument \"key\" is missing")
})
