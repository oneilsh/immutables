testthat::test_that("Monoid can be stored on the tree", {
  r <- Monoid(function(a, b) a + b, 0)
  t <- tree_from(1:3, monoid = r)
  testthat::expect_identical(reduce_left(t), 6)
  testthat::expect_identical(reduce_right(t), 6)
})

testthat::test_that("Monoid propagates through prepend/append/concat", {
  r <- Monoid(function(a, b) a + b, 0)
  t <- tree_from(1:2, monoid = r)
  t <- prepend(t, 0)
  t <- append(t, 3)
  testthat::expect_identical(reduce_left(t), 6)

  t2 <- tree_from(4:5, monoid = r)
  t3 <- concat_trees(t, t2)
  testthat::expect_identical(reduce_left(t3), 15)
})

testthat::test_that("Reduce errors when no monoid is available", {
  t <- tree_from(1:3)
  testthat::expect_error(reduce_left(t), "No monoid provided")
  testthat::expect_error(reduce_right(t), "No monoid provided")
})
