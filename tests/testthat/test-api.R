testthat::test_that("MeasureMonoid can be stored on the tree", {
  r <- MeasureMonoid(function(a, b) a + b, 0, function(el) 0)
  t <- tree_from(1:3, monoid = r)
  testthat::expect_identical(reduce_left(t), 6)
  testthat::expect_identical(reduce_right(t), 6)
})

testthat::test_that("MeasureMonoid propagates through prepend/append/concat", {
  r <- MeasureMonoid(function(a, b) a + b, 0, function(el) 0)
  t <- tree_from(1:2, monoid = r)
  t <- prepend(t, 0)
  t <- append(t, 3)
  testthat::expect_identical(reduce_left(t), 6)

  t2 <- tree_from(4:5, monoid = r)
  t3 <- concat_trees(t, t2)
  testthat::expect_identical(reduce_left(t3), 15)
})

testthat::test_that("Tree construction requires MeasureMonoid", {
  testthat::expect_error(tree_from(1:3), "No monoid provided")
  testthat::expect_error(empty_tree(), "No monoid provided")
})

testthat::test_that("concat_trees resolves monoid from either side and checks compatibility", {
  r <- MeasureMonoid(function(a, b) a + b, 0, function(el) 0)
  t1 <- tree_from(1:2, monoid = r)
  t2 <- tree_from(3:4, monoid = r)
  attr(t1, "monoid") <- NULL
  t3 <- concat_trees(t1, t2)
  testthat::expect_identical(reduce_left(t3), 10)

  r2 <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t4 <- tree_from(1:2, monoid = r)
  t5 <- tree_from(3:4, monoid = r2)
  testthat::expect_error(concat_trees(t4, t5), "different monoid attributes")
})
