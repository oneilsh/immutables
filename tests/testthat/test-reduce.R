testthat::test_that("Single monoids do not reapply identity", {
  r <- Monoid(function(a, b) paste0("(", a, ",", b, ")"), "i")
  s <- prepend(empty_tree(), "a")

  testthat::expect_identical(reduce_left(s, r), "(i,a)")
  testthat::expect_identical(reduce_right(s, r), "(a,i)")
})

testthat::test_that("Reduce order is correct for left and right folds", {
  r <- Monoid(function(a, b) paste0("(", a, ",", b, ")"), "i")
  t <- tree_from(list("a", "b"))

  testthat::expect_identical(reduce_left(t, r), "((i,a),b)")
  testthat::expect_identical(reduce_right(t, r), "(a,(b,i))")
})

testthat::test_that("Concat preserves element order", {
  r <- Monoid(function(a, b) paste0(a, b), "")
  t1 <- tree_from(list("a", "b"))
  t2 <- tree_from(list("c", "d"))
  t <- concat_trees(t1, t2)

  testthat::expect_identical(reduce_left(t, r), "abcd")
})

testthat::test_that("Tree construction order matches base list order", {
  r <- Monoid(function(a, b) paste0(a, b), "")
  v <- list("x", "y", "z", "w")
  t <- tree_from(v)
  testthat::expect_identical(reduce_left(t, r), "xyzw")
})
