# Benchmark append() workloads with progress output every 100 inserts.
# Run from repo root, e.g.:
#   source("meta/bench.R")
#   run_all_benches(n = 2000, use_cpp = TRUE)

if(requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
}

bench_default <- function(n = 2000, use_cpp = TRUE) {
  message("== default monoids, unnamed elements ==")
  print(system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- empty_tree()
    for(i in seq_len(n)) {
      t <- append(t, i)
      if(i %% 100 == 0) print(i)
    }
  }))
}

bench_custom_monoid <- function(n = 2000, use_cpp = TRUE) {
  message("== custom monoid, unnamed elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  print(system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- empty_tree(monoids = list(sum = sum_m))
    for(i in seq_len(n)) {
      t <- append(t, i)
      if(i %% 100 == 0) print(i)
    }
  }))
}

bench_named <- function(n = 2000, use_cpp = TRUE) {
  message("== default monoids, named elements ==")
  print(system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- empty_tree()
    for(i in seq_len(n)) {
      t <- append(t, stats::setNames(i, paste0("k", i)))
      if(i %% 100 == 0) print(i)
    }
  }))
}

bench_named_custom_monoid <- function(n = 2000, use_cpp = TRUE) {
  message("== custom monoid, named elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  print(system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- empty_tree(monoids = list(sum = sum_m))
    for(i in seq_len(n)) {
      t <- append(t, stats::setNames(i, paste0("k", i)))
      if(i %% 100 == 0) print(i)
    }
  }))
}

run_all_benches <- function(n = 2000, use_cpp = TRUE) {
  bench_default(n = n, use_cpp = use_cpp)
  bench_custom_monoid(n = n, use_cpp = use_cpp)
  bench_named(n = n, use_cpp = use_cpp)
  bench_named_custom_monoid(n = n, use_cpp = use_cpp)
}

