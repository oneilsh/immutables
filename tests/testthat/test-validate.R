testthat::test_that("validate utilities pass on valid trees", {
  t <- tree_from(letters[1:12])
  testthat::expect_invisible(validate_tree(t))
  testthat::expect_invisible(validate_name_state(t))
})

testthat::test_that("validate_name_state catches mixed named/unnamed trees", {
  t <- tree_from(letters[1:3])
  # Force invalid named-count metadata for validation-path coverage only.
  bad <- t
  ms <- attr(bad, "measures", exact = TRUE)
  ms[[".named_count"]] <- as.integer(ms[[".size"]])
  attr(bad, "measures") <- ms
  testthat::expect_error(validate_name_state(bad), "non-empty")
})
