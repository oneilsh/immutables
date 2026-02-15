testthat::test_that("Single monoids do not reapply identity", {
  r <- measure_monoid(function(a, b) paste0("(", a, ",", b, ")"), "i", function(el) "")
  s <- prepend(flexseq(), "a")

  testthat::expect_identical(fold_left(s, r), "(i,a)")
  testthat::expect_identical(fold_right(s, r), "(a,i)")
})

testthat::test_that("Reduce order is correct for left and right folds", {
  r <- measure_monoid(function(a, b) paste0("(", a, ",", b, ")"), "i", function(el) "")
  t <- as_flexseq(list("a", "b"))

  testthat::expect_identical(fold_left(t, r), "((i,a),b)")
  testthat::expect_identical(fold_right(t, r), "(a,(b,i))")
})

testthat::test_that("Concat preserves element order", {
  r <- measure_monoid(function(a, b) paste0(a, b), "", function(el) "")
  t1 <- as_flexseq(list("a", "b"))
  t2 <- as_flexseq(list("c", "d"))
  t <- c(t1, t2)

  testthat::expect_identical(fold_left(t, r), "abcd")
})

testthat::test_that("Tree construction order matches base list order", {
  r <- measure_monoid(function(a, b) paste0(a, b), "", function(el) "")
  v <- list("x", "y", "z", "w")
  t <- as_flexseq(v)
  testthat::expect_identical(fold_left(t, r), "xyzw")
})
