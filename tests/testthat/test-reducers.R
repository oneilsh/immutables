testthat::test_that("Single reducers do not reapply identity", {
  r <- Reducer(function(a, b) paste0("(", a, ",", b, ")"), "i")
  s <- Single("a")

  testthat::expect_identical(reduce_left(s, r), "(i,a)")
  testthat::expect_identical(reduce_right(s, r), "(a,i)")
})

testthat::test_that("Reduce order is correct for left and right folds", {
  r <- Reducer(function(a, b) paste0("(", a, ",", b, ")"), "i")
  t <- as.FingerTree(list("a", "b"))

  testthat::expect_identical(reduce_left(t, r), "((i,a),b)")
  testthat::expect_identical(reduce_right(t, r), "(a,(b,i))")
})

testthat::test_that("Concat preserves element order", {
  r <- Reducer(function(a, b) paste0(a, b), "")
  t1 <- as.FingerTree(list("a", "b"))
  t2 <- as.FingerTree(list("c", "d"))
  t <- concat(t1, t2)

  testthat::expect_identical(reduce_left(t, r), "abcd")
})

testthat::test_that("Elements can be lists and data.frames", {
  r <- Reducer(function(a, b) c(a, list(b)), list())
  df <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
  t <- Empty()
  t <- add_right(t, list(a = 1, b = 2))
  t <- add_right(t, df)

  reduced <- reduce_left(t, r)
  testthat::expect_identical(length(reduced), 2L)
  testthat::expect_true(is.list(reduced[[1]]))
  testthat::expect_true(is.data.frame(reduced[[2]]))
})
