testthat::test_that("Single monoids do not reapply identity", {
  r <- MeasureMonoid(function(a, b) paste0("(", a, ",", b, ")"), "i", function(el) "")
  s <- prepend(empty_tree(r), "a")

  testthat::expect_identical(reduce_left(s, r), "(i,a)")
  testthat::expect_identical(reduce_right(s, r), "(a,i)")
})

testthat::test_that("Reduce order is correct for left and right folds", {
  r <- MeasureMonoid(function(a, b) paste0("(", a, ",", b, ")"), "i", function(el) "")
  t <- tree_from(list("a", "b"), monoid = r)

  testthat::expect_identical(reduce_left(t, r), "((i,a),b)")
  testthat::expect_identical(reduce_right(t, r), "(a,(b,i))")
})

testthat::test_that("Concat preserves element order", {
  r <- MeasureMonoid(function(a, b) paste0(a, b), "", function(el) "")
  t1 <- tree_from(list("a", "b"), monoid = r)
  t2 <- tree_from(list("c", "d"), monoid = r)
  t <- concat_trees(t1, t2)

  testthat::expect_identical(reduce_left(t, r), "abcd")
})

testthat::test_that("Tree construction order matches base list order", {
  r <- MeasureMonoid(function(a, b) paste0(a, b), "", function(el) "")
  v <- list("x", "y", "z", "w")
  t <- tree_from(v, monoid = r)
  testthat::expect_identical(reduce_left(t), "xyzw")
})
