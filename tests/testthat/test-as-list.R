testthat::test_that("as.list.flexseq returns elements in sequence order", {
  x <- as_flexseq(letters[1:5])
  out <- as.list(x)

  testthat::expect_type(out, "list")
  testthat::expect_identical(out, as.list(letters[1:5]))
})

testthat::test_that("as.list.flexseq preserves element names", {
  x <- as_flexseq(setNames(as.list(1:4), c("a", "b", "c", "d")))
  out <- as.list(x)

  testthat::expect_identical(names(out), c("a", "b", "c", "d"))
  testthat::expect_identical(unname(out), as.list(1:4))
})

testthat::test_that("as.list.flexseq strips internal ft_name metadata", {
  x <- as_flexseq(setNames(as.list(1:3), c("x", "y", "z")))
  out <- as.list(x)

  testthat::expect_null(attr(out[[1]], "ft_name", exact = TRUE))
  testthat::expect_null(attr(out[[2]], "ft_name", exact = TRUE))
  testthat::expect_null(attr(out[[3]], "ft_name", exact = TRUE))
})
