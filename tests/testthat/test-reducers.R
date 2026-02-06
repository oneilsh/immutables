testthat::test_that("Single reducers do not reapply identity", {
  r <- Reducer(function(a, b) paste0("(", a, ",", b, ")"), "i")
  e <- Element("a")
  s <- Single(e)

  testthat::expect_identical(reduce_left(s, r), reduce_left(e, r))
  testthat::expect_identical(reduce_right(s, r), reduce_right(e, r))
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
