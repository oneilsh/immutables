# Benchmark append() workloads with progress output every 100 inserts.
# Run from repo root, e.g.:
#   source("meta/bench.R")
#   run_all_benches(n = 2000, use_cpp = TRUE)

if(requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
}

bench_default <- function(n = 2000, use_cpp = TRUE) {
  message("== default monoids, unnamed elements ==")
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- empty_tree()
    for(i in seq_len(n)) {
      t <- append(t, i)
      if(i %% 100 == 0) print(i)
    }
  })
  print(timing)
  timing
}

bench_custom_monoid <- function(n = 2000, use_cpp = TRUE) {
  message("== custom monoid, unnamed elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- empty_tree(monoids = list(sum = sum_m))
    for(i in seq_len(n)) {
      t <- append(t, i)
      if(i %% 100 == 0) print(i)
    }
  })
  print(timing)
  timing
}

bench_named <- function(n = 2000, use_cpp = TRUE) {
  message("== default monoids, named elements ==")
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- empty_tree()
    for(i in seq_len(n)) {
      t <- append(t, stats::setNames(i, paste0("k", i)))
      if(i %% 100 == 0) print(i)
    }
  })
  print(timing)
  timing
}

bench_named_custom_monoid <- function(n = 2000, use_cpp = TRUE) {
  message("== custom monoid, named elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- empty_tree(monoids = list(sum = sum_m))
    for(i in seq_len(n)) {
      t <- append(t, stats::setNames(i, paste0("k", i)))
      if(i %% 100 == 0) print(i)
    }
  })
  print(timing)
  timing
}

run_all_benches <- function(n = 2000, use_cpp = TRUE) {
  out <- list(
    default = bench_default(n = n, use_cpp = use_cpp),
    custom_monoid = bench_custom_monoid(n = n, use_cpp = use_cpp),
    named = bench_named(n = n, use_cpp = use_cpp),
    named_custom_monoid = bench_named_custom_monoid(n = n, use_cpp = use_cpp)
  )
  out
}
