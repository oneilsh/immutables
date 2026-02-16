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
  base_names <- paste0("k", seq_len(8))

  x_oms <- as_ordered_multiset(list("aa", "bb", "c", "ddd"), keys = c(2, 2, 1, 3))
  y_oms <- as_ordered_multiset(list("xx", "z", "qq", "rrrr"), keys = c(2, 1, 2, 4))
  ms_oms <- attr(x_oms, "monoids", exact = TRUE)
  key_type_oms <- attr(x_oms, "oms_key_type", exact = TRUE)

  options(immutables.use_cpp = TRUE)
  old_merge_engine <- getOption("immutables.oms.merge_engine")
  options(immutables.oms.merge_engine = "auto")
  on.exit({
    if(is.null(old_merge_engine)) {
      options(immutables.oms.merge_engine = NULL)
    } else {
      options(immutables.oms.merge_engine = old_merge_engine)
    }
  }, add = TRUE)

  old_torture <- gctorture2(1, inhibit_release = FALSE)
  on.exit(gctorture2(old_torture, inhibit_release = FALSE), add = TRUE)

  step <- function(label, expr) {
    tryCatch(
      force(expr),
      error = function(e) {
        stop(sprintf("%s failed: %s", label, conditionMessage(e)), call. = FALSE)
      }
    )
  }

  testthat::expect_no_error({
    t <- NULL
    step("ft_cpp_tree_from", t <- .ft_cpp_tree_from(as.list(1:8), ms))
    step("ft_cpp_tree_from_prepared", .ft_cpp_tree_from_prepared(as.list(1:8), base_names, ms))
    step("ft_cpp_tree_from_sorted", .ft_cpp_tree_from_sorted(as.list(1:8), ms))
    step("ft_cpp_add_right", t <- .ft_cpp_add_right(t, 9, ms))
    step("ft_cpp_add_left", t <- .ft_cpp_add_left(t, 0, ms))
    step("ft_cpp_concat", .ft_cpp_concat(base_plain, base_plain, ms))
    step("ft_cpp_locate", .ft_cpp_locate(base_plain, function(v) v >= 4, ms, ".size", 0))
    step("ft_cpp_split_tree", .ft_cpp_split_tree(base_plain, function(v) v >= 4, ms, ".size", 0))
    step("ft_cpp_oms_insert", .ft_cpp_oms_insert(x_oms, list(item = "new", key = 2, seq_id = 100), ms_oms, key_type_oms))
    step("ft_cpp_oms_set_merge", .ft_cpp_oms_set_merge(x_oms, y_oms, "union", ms_oms, key_type_oms))
    step("insert_ms", insert_ms(x_oms, "newer", key = 2))
    step("union_ms", union_ms(x_oms, y_oms))
  })
})
