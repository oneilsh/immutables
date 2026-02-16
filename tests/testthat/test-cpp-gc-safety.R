testthat::test_that("C++ core tree operations survive GC torture", {
  gc_stress <- tolower(Sys.getenv("IMMUTABLES_GC_STRESS", unset = "0"))
  if(!(gc_stress %in% c("1", "true", "yes", "on"))) {
    testthat::skip("Set IMMUTABLES_GC_STRESS=1 to run GC torture regression.")
  }

  old_cpp <- getOption("immutables.use_cpp")
  options(immutables.use_cpp = FALSE)
  on.exit({
    if(is.null(old_cpp)) {
      options(immutables.use_cpp = NULL)
    } else {
      options(immutables.use_cpp = old_cpp)
    }
  }, add = TRUE)

  base_plain <- as_flexseq(as.list(1:8))
  ms <- attr(base_plain, "monoids", exact = TRUE)

  options(immutables.use_cpp = TRUE)

  old_torture <- gctorture2(1, inhibit_release = FALSE)
  on.exit(gctorture2(old_torture, inhibit_release = FALSE), add = TRUE)

  testthat::expect_no_error({
    t <- .ft_cpp_tree_from(as.list(1:8), ms)
    t <- .ft_cpp_add_right(t, 9, ms)
    t <- .ft_cpp_add_left(t, 0, ms)

    .ft_cpp_concat(base_plain, base_plain, ms)
    .ft_cpp_locate(base_plain, function(v) v >= 4, ms, ".size", 0)
    .ft_cpp_split_tree(base_plain, function(v) v >= 4, ms, ".size", 0)
  })
})
