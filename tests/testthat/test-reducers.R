testthat::test_that("Single reducers do not reapply identity", {
  r <- Reducer(function(a, b) paste0("(", a, ",", b, ")"), "i")
  s <- prepend(empty_tree(), "a")

  testthat::expect_identical(reduce_left(s, r), "(i,a)")
  testthat::expect_identical(reduce_right(s, r), "(a,i)")
})

testthat::test_that("Reduce order is correct for left and right folds", {
  r <- Reducer(function(a, b) paste0("(", a, ",", b, ")"), "i")
  t <- tree_from(list("a", "b"))

  testthat::expect_identical(reduce_left(t, r), "((i,a),b)")
  testthat::expect_identical(reduce_right(t, r), "(a,(b,i))")
})

testthat::test_that("Concat preserves element order", {
  r <- Reducer(function(a, b) paste0(a, b), "")
  t1 <- tree_from(list("a", "b"))
  t2 <- tree_from(list("c", "d"))
  t <- concat_trees(t1, t2)

  testthat::expect_identical(reduce_left(t, r), "abcd")
})

testthat::test_that("Elements can be lists and data.frames", {
  r <- Reducer(function(a, b) c(a, list(b)), list())
  df <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
  t <- empty_tree()
  t <- append(t, list(a = 1, b = 2))
  t <- append(t, df)

  reduced <- reduce_left(t, r)
  testthat::expect_identical(length(reduced), 2L)
  testthat::expect_true(is.list(reduced[[1]]))
  testthat::expect_true(is.data.frame(reduced[[2]]))
})

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

testthat::test_that("Tree construction order matches base list order", {
  r <- Reducer(function(a, b) paste0(a, b), "")
  v <- list("x", "y", "z", "w")
  t <- tree_from(v)
  testthat::expect_identical(reduce_left(t, r), "xyzw")
})
