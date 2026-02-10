validate_fingertree_invariants <- function(t) {
  is_homogeneous <- function(xs) {
    if(length(xs) <= 1) {
      return(TRUE)
    }
    kinds <- vapply(xs, is_structural_node, logical(1))
    all(kinds == kinds[[1]])
  }

  walk <- function(x) {
    if(x %isa% Empty) {
      return(invisible(TRUE))
    }

    if(x %isa% Single) {
      testthat::expect_identical(length(x), 1L)
      return(invisible(TRUE))
    }

    if(x %isa% Deep) {
      testthat::expect_true(length(x$prefix) >= 1 && length(x$prefix) <= 4)
      testthat::expect_true(length(x$suffix) >= 1 && length(x$suffix) <= 4)
      testthat::expect_true(is_homogeneous(x$prefix))
      testthat::expect_true(is_homogeneous(x$suffix))
      walk(x$middle)
      return(invisible(TRUE))
    }

    if(x %isa% Node) {
      testthat::expect_true(length(x) %in% c(2L, 3L))
      testthat::expect_true(is_homogeneous(x))
      for(el in x) {
        if(is_structural_node(el)) {
          walk(el)
        }
      }
      return(invisible(TRUE))
    }

    if(x %isa% Digit) {
      testthat::expect_true(length(x) >= 1 && length(x) <= 4)
      testthat::expect_true(is_homogeneous(x))
      for(el in x) {
        if(is_structural_node(el)) {
          walk(el)
        }
      }
      return(invisible(TRUE))
    }
  }

  walk(t)
  invisible(TRUE)
}

testthat::test_that("structural invariants hold after mixed updates", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- empty_tree(mr)
  for(i in 1:30) {
    if(i %% 2 == 0) {
      t <- append(t, letters[i %% 26 + 1], mr)
    } else {
      t <- prepend(t, letters[i %% 26 + 1], mr)
    }
  }
  validate_fingertree_invariants(t)
})

testthat::test_that("structural invariants hold after concat and split", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t1 <- tree_from(letters[1:12], monoid = mr)
  t2 <- tree_from(letters[13:20], monoid = mr)
  t <- concat_trees(t1, t2)
  validate_fingertree_invariants(t)

  s <- split(t, function(v) v >= 10)
  validate_fingertree_invariants(s$left)
  validate_fingertree_invariants(s$right)
})

testthat::test_that("measured deep trees retain root measure through updates", {
  mr <- MeasureMonoid(function(a, b) a + b, 0, function(el) 1)
  t <- tree_from(letters[1:10], monoid = mr)
  t <- prepend(t, "z")
  t <- append(t, "y")
  testthat::expect_identical(attr(t, "measure"), 12)
  validate_fingertree_invariants(t)
})
