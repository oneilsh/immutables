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

bench_tree_from_default <- function(n = 2000, use_cpp = TRUE) {
  message("== tree_from default monoids, unnamed elements ==")
  x <- as.list(seq_len(n))
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- tree_from(x)
  })
  print(timing)
  timing
}

bench_tree_from_custom_monoid <- function(n = 2000, use_cpp = TRUE) {
  message("== tree_from custom monoid, unnamed elements ==")
  x <- as.list(seq_len(n))
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- tree_from(x, monoids = list(sum = sum_m))
  })
  print(timing)
  timing
}

bench_tree_from_named <- function(n = 2000, use_cpp = TRUE) {
  message("== tree_from default monoids, named elements ==")
  x <- as.list(seq_len(n))
  names(x) <- paste0("k", seq_len(n))
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- tree_from(x)
  })
  print(timing)
  timing
}

bench_tree_from_named_custom_monoid <- function(n = 2000, use_cpp = TRUE) {
  message("== tree_from custom monoid, named elements ==")
  x <- as.list(seq_len(n))
  names(x) <- paste0("k", seq_len(n))
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    t <- tree_from(x, monoids = list(sum = sum_m))
  })
  print(timing)
  timing
}

run_all_benches <- function(n = 2000, use_cpp = TRUE) {
  out <- list(
    #append_default = bench_default(n = n, use_cpp = use_cpp),
    #append_custom = bench_custom_monoid(n = n, use_cpp = use_cpp),
    #append_named = bench_named(n = n, use_cpp = use_cpp),
    #append_named_custom = bench_named_custom_monoid(n = n, use_cpp = use_cpp),
    from_default = bench_tree_from_default(n = n, use_cpp = use_cpp),
    from_custom = bench_tree_from_custom_monoid(n = n, use_cpp = use_cpp),
    from_named = bench_tree_from_named(n = n, use_cpp = use_cpp),
    from_named_custom = bench_tree_from_named_custom_monoid(n = n, use_cpp = use_cpp)
  )
  out
}

# don't remove these!
devtools::document()
devtools::load_all()

with_cpp <- run_all_benches(use_cpp = TRUE)
no_cpp <- run_all_benches(use_cpp = FALSE)

print(as.data.frame(with_cpp))
print(as.data.frame(no_cpp))