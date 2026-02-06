testthat::test_that("Reducer can be stored on the tree", {
  r <- Reducer(function(a, b) a + b, 0)
  t <- tree_from(1:3, reducer = r)
  testthat::expect_identical(reduce_left(t), 6)
  testthat::expect_identical(reduce_right(t), 6)
})

testthat::test_that("Reducer propagates through prepend/append/concat", {
  r <- Reducer(function(a, b) a + b, 0)
  t <- tree_from(1:2, reducer = r)
  t <- prepend(t, 0)
  t <- append(t, 3)
  testthat::expect_identical(reduce_left(t), 6)

  t2 <- tree_from(4:5, reducer = r)
  t3 <- concat_trees(t, t2)
  testthat::expect_identical(reduce_left(t3), 15)
})

testthat::test_that("Reduce errors when no reducer is available", {
  t <- tree_from(1:3)
  testthat::expect_error(reduce_left(t), "No reducer provided")
  testthat::expect_error(reduce_right(t), "No reducer provided")
})
