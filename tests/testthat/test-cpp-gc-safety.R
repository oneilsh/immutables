.gc_stress_enabled <- function() {
  gc_stress <- tolower(Sys.getenv("IMMUTABLES_GC_STRESS", unset = "0"))
  gc_stress %in% c("1", "true", "yes", "on")
}

.skip_if_no_gc_stress <- function() {
  if(!isTRUE(.gc_cpp_run$enabled)) {
    testthat::skip("Set IMMUTABLES_GC_STRESS=1 to run GC torture regression.")
  }
}

.run_gc_stress_suite <- function() {
  if(!.gc_stress_enabled()) {
    return(list(enabled = FALSE, setup_error = NULL, results = list()))
  }

  results <- list()
  setup_error <- NULL

  tryCatch({
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

    x_oms <- as_ordered_sequence(list("aa", "bb", "c", "ddd"), keys = c(2, 2, 1, 3))
    y_oms <- as_ordered_sequence(list("xx", "z", "qq", "rrrr"), keys = c(2, 1, 2, 4))
    ms_oms <- attr(x_oms, "monoids", exact = TRUE)
    key_type_oms <- attr(x_oms, "oms_key_type", exact = TRUE)

    options(immutables.use_cpp = TRUE)

    old_torture <- gctorture2(1, inhibit_release = FALSE)
    on.exit(gctorture2(old_torture, inhibit_release = FALSE), add = TRUE)

    step <- function(label, expr) {
      results[[label]] <<- tryCatch(
        {
          force(expr)
          NULL
        },
        error = function(e) conditionMessage(e)
      )
      invisible(NULL)
    }

    t <- NULL
    step("ft_cpp_tree_from", t <- .ft_cpp_tree_from(as.list(1:8), ms))
    step("ft_cpp_tree_from_prepared", .ft_cpp_tree_from_prepared(as.list(1:8), base_names, ms))
    step("ft_cpp_tree_from_sorted", .ft_cpp_tree_from_sorted(as.list(1:8), ms))

    if(is.null(results[["ft_cpp_tree_from"]])) {
      step("ft_cpp_add_right", t <- .ft_cpp_add_right(t, 9, ms))
      step("ft_cpp_add_left", t <- .ft_cpp_add_left(t, 0, ms))
    } else {
      results[["ft_cpp_add_right"]] <- "skipped because ft_cpp_tree_from failed"
      results[["ft_cpp_add_left"]] <- "skipped because ft_cpp_tree_from failed"
    }

    step("ft_cpp_concat", .ft_cpp_concat(base_plain, base_plain, ms))
    step("ft_cpp_locate", .ft_cpp_locate(base_plain, function(v) v >= 4, ms, ".size", 0))
    step("ft_cpp_split_tree", .ft_cpp_split_tree(base_plain, function(v) v >= 4, ms, ".size", 0))

    step("ft_cpp_oms_insert", .ft_cpp_oms_insert(x_oms, list(item = "new", key = 2), ms_oms, key_type_oms))
    step("ft_cpp_oms_set_merge", .ft_cpp_oms_set_merge(x_oms, y_oms, "union", ms_oms, key_type_oms))

    step("insert", insert(x_oms, "newer", key = 2))
  }, error = function(e) {
    setup_error <<- conditionMessage(e)
  })

  list(enabled = TRUE, setup_error = setup_error, results = results)
}

.gc_cpp_run <- .run_gc_stress_suite()

.expect_step_ok <- function(label) {
  .skip_if_no_gc_stress()

  if(!is.null(.gc_cpp_run$setup_error)) {
    testthat::fail(sprintf("GC stress suite setup failed: %s", .gc_cpp_run$setup_error))
  }

  err <- .gc_cpp_run$results[[label]]
  if(is.null(err)) {
    testthat::succeed()
  } else {
    testthat::fail(sprintf("%s failed: %s", label, err))
  }
}

testthat::test_that("C++ tree builders survive GC torture", {
  .expect_step_ok("ft_cpp_tree_from")
  .expect_step_ok("ft_cpp_tree_from_prepared")
  .expect_step_ok("ft_cpp_tree_from_sorted")
})

testthat::test_that("C++ core tree ops survive GC torture", {
  .expect_step_ok("ft_cpp_add_right")
  .expect_step_ok("ft_cpp_add_left")
  .expect_step_ok("ft_cpp_concat")
  .expect_step_ok("ft_cpp_locate")
  .expect_step_ok("ft_cpp_split_tree")
})

testthat::test_that("C++ OMS primitives survive GC torture", {
  .expect_step_ok("ft_cpp_oms_insert")
  .expect_step_ok("ft_cpp_oms_set_merge")
})

testthat::test_that("OMS public APIs survive GC torture", {
  .expect_step_ok("insert")
})
