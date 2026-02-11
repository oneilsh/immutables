testthat::test_that("reduce uses explicit monoid object", {
  r <- MeasureMonoid(function(a, b) a + b, 0, function(el) 0)
  t <- tree_from(1:3)
  testthat::expect_identical(reduce_left(t, r), 6)
  testthat::expect_identical(reduce_right(t, r), 6)
})

testthat::test_that("tree construction defaults to .size monoid", {
  t <- tree_from(1:3)
  ms <- attr(t, "measures")
  testthat::expect_identical(ms$.size, 3)
  testthat::expect_true(!is.null(attr(t, "monoids")[[".size"]]))

  e <- empty_tree()
  testthat::expect_true(e %isa% Empty)
  testthat::expect_identical(attr(e, "measures")$.size, 0)
})

testthat::test_that("add_monoids merges and supports overwrite flag", {
  t <- tree_from(1:5)
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) el)
  t2 <- add_monoids(t, list(sum = sum_m))

  testthat::expect_identical(attr(t2, "measures")$sum, 15)
  testthat::expect_identical(attr(t2, "measures")$.size, 5)

  sum2 <- MeasureMonoid(function(a, b) a + b + 1, 0, function(el) el)
  testthat::expect_error(
    add_monoids(t2, list(sum = sum2), overwrite = FALSE),
    "already exist"
  )

  t3 <- add_monoids(t2, list(sum = sum2), overwrite = TRUE)
  testthat::expect_true(!identical(attr(t2, "measures")$sum, attr(t3, "measures")$sum))
})

testthat::test_that("concat_trees unions monoids and warns on shared names", {
  a <- MeasureMonoid(function(x, y) x + y, 0, function(el) el)
  b <- MeasureMonoid(function(x, y) x + y, 0, function(el) 1)

  t1 <- tree_from(1:2, monoids = list(sum = a))
  t2 <- tree_from(3:4, monoids = list(cnt = b))
  t <- concat_trees(t1, t2)
  testthat::expect_true(all(c(".size", "sum", "cnt") %in% names(attr(t, "monoids"))))

  # shared name warning path: left definition is assumed authoritative
  t3 <- tree_from(1:2, monoids = list(sum = a))
  t4 <- tree_from(3:4, monoids = list(sum = MeasureMonoid(function(x, y) x + y, 0, function(el) 1)))
  testthat::expect_warning(
    concat_trees(t3, t4),
    class = "fingertree_monoid_assumption_warning"
  )
})
