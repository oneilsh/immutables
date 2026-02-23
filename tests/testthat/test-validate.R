testthat::test_that("validate utilities pass on valid trees", {
  t <- as_flexseq(letters[1:12])
  tree_check <- withVisible(validate_tree(t))
  name_check <- withVisible(validate_name_state(t))

  testthat::expect_false(tree_check$visible)
  testthat::expect_false(name_check$visible)
  testthat::expect_true(isTRUE(tree_check$value))
  testthat::expect_true(isTRUE(name_check$value))
})

testthat::test_that("validate_name_state catches mixed named/unnamed trees", {
  t <- as_flexseq(letters[1:3])
  # Force invalid named-count metadata for validation-path coverage only.
  bad <- t
  ms <- attr(bad, "measures", exact = TRUE)
  ms[[".named_count"]] <- as.integer(ms[[".size"]])
  attr(bad, "measures") <- ms
  testthat::expect_error(validate_name_state(bad), "non-empty")
})
