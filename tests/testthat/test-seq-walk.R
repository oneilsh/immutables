testthat::test_that("seq_walk visits elements in order and returns x invisibly", {
  x <- as_flexseq(1:5)
  seen <- integer(0)

  ret <- seq_walk(x, function(v) {
    seen <<- c(seen, v)
  })

  testthat::expect_identical(seen, 1:5)
  testthat::expect_identical(ret, x)
})

testthat::test_that("seq_walk validates inputs", {
  x <- as_flexseq(1:3)
  testthat::expect_error(seq_walk(list(1, 2, 3), identity), "`x` must be a flexseq")
  testthat::expect_error(seq_walk(x, 1), "`f` must be a function")
})
