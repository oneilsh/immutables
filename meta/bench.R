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

bench_concat_default <- function(n = 2000, reps = 500, use_cpp = TRUE) {
  message("== concat default monoids, unnamed elements ==")
  left <- tree_from(as.list(seq_len(n)))
  right <- tree_from(as.list(seq_len(n) + n))
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(i in seq_len(reps)) {
      t <- concat_trees(left, right)
      if(i %% 100 == 0) print(i)
    }
  })
  print(timing)
  timing
}

bench_concat_custom_monoid <- function(n = 2000, reps = 500, use_cpp = TRUE) {
  message("== concat custom monoid, unnamed elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  left <- tree_from(as.list(seq_len(n)), monoids = list(sum = sum_m))
  right <- tree_from(as.list(seq_len(n) + n), monoids = list(sum = sum_m))
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(i in seq_len(reps)) {
      t <- suppressWarnings(concat_trees(left, right))
      if(i %% 100 == 0) print(i)
    }
  })
  print(timing)
  timing
}

bench_concat_named <- function(n = 2000, reps = 500, use_cpp = TRUE) {
  message("== concat default monoids, named elements ==")
  left_vals <- as.list(seq_len(n))
  names(left_vals) <- paste0("l", seq_len(n))
  right_vals <- as.list(seq_len(n) + n)
  names(right_vals) <- paste0("r", seq_len(n))
  left <- tree_from(left_vals)
  right <- tree_from(right_vals)
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(i in seq_len(reps)) {
      t <- concat_trees(left, right)
      if(i %% 100 == 0) print(i)
    }
  })
  print(timing)
  timing
}

bench_concat_named_custom_monoid <- function(n = 2000, reps = 500, use_cpp = TRUE) {
  message("== concat custom monoid, named elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  left_vals <- as.list(seq_len(n))
  names(left_vals) <- paste0("l", seq_len(n))
  right_vals <- as.list(seq_len(n) + n)
  names(right_vals) <- paste0("r", seq_len(n))
  left <- tree_from(left_vals, monoids = list(sum = sum_m))
  right <- tree_from(right_vals, monoids = list(sum = sum_m))
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(i in seq_len(reps)) {
      t <- suppressWarnings(concat_trees(left, right))
      if(i %% 100 == 0) print(i)
    }
  })
  print(timing)
  timing
}

bench_locate_default <- function(n = 3000, queries = 200, use_cpp = TRUE) {
  message("== locate default monoids, unnamed elements ==")
  t <- tree_from(as.list(seq_len(n)))
  idx <- sample.int(n, queries, replace = TRUE)
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(i in idx) {
      x <- locate(t, function(v) v >= i, ".size")
    }
  })
  print(timing)
  timing
}

bench_locate_custom_monoid <- function(n = 3000, queries = 200, use_cpp = TRUE) {
  message("== locate custom monoid, unnamed elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  t <- tree_from(as.list(seq_len(n)), monoids = list(sum = sum_m))
  idx <- sample.int(n, queries, replace = TRUE)
  targets <- cumsum(seq_len(n))[idx]
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(target in targets) {
      x <- locate(t, function(v) v >= target, "sum")
    }
  })
  print(timing)
  timing
}

bench_locate_custom_monoid_named <- function(n = 3000, queries = 200, use_cpp = TRUE) {
  message("== locate custom monoid, named elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  vals <- as.list(seq_len(n))
  names(vals) <- paste0("k", seq_len(n))
  t <- tree_from(vals, monoids = list(sum = sum_m))
  idx <- sample.int(n, queries, replace = TRUE)
  targets <- cumsum(seq_len(n))[idx]
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(target in targets) {
      x <- locate(t, function(v) v >= target, "sum")
    }
  })
  print(timing)
  timing
}

bench_split_tree_default <- function(n = 3000, queries = 200, use_cpp = TRUE) {
  message("== split_tree default monoids, unnamed elements ==")
  t <- tree_from(as.list(seq_len(n)))
  idx <- sample.int(n, queries, replace = TRUE)
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(i in idx) {
      s <- split_tree(t, function(v) v >= i, ".size")
    }
  })
  print(timing)
  timing
}

bench_split_tree_custom_monoid <- function(n = 3000, queries = 200, use_cpp = TRUE) {
  message("== split_tree custom monoid, unnamed elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  t <- tree_from(as.list(seq_len(n)), monoids = list(sum = sum_m))
  idx <- sample.int(n, queries, replace = TRUE)
  targets <- cumsum(seq_len(n))[idx]
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(target in targets) {
      s <- split_tree(t, function(v) v >= target, "sum")
    }
  })
  print(timing)
  timing
}

bench_split_tree_custom_monoid_named <- function(n = 3000, queries = 200, use_cpp = TRUE) {
  message("== split_tree custom monoid, named elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  vals <- as.list(seq_len(n))
  names(vals) <- paste0("k", seq_len(n))
  t <- tree_from(vals, monoids = list(sum = sum_m))
  idx <- sample.int(n, queries, replace = TRUE)
  targets <- cumsum(seq_len(n))[idx]
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(target in targets) {
      s <- split_tree(t, function(v) v >= target, "sum")
    }
  })
  print(timing)
  timing
}

bench_split_default <- function(n = 3000, queries = 200, use_cpp = TRUE) {
  message("== split default monoids, unnamed elements ==")
  t <- tree_from(as.list(seq_len(n)))
  idx <- sample.int(n, queries, replace = TRUE)
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(i in idx) {
      s <- split(t, function(v) v >= i, ".size")
    }
  })
  print(timing)
  timing
}

bench_split_custom_monoid <- function(n = 3000, queries = 200, use_cpp = TRUE) {
  message("== split custom monoid, unnamed elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  t <- tree_from(as.list(seq_len(n)), monoids = list(sum = sum_m))
  idx <- sample.int(n, queries, replace = TRUE)
  targets <- cumsum(seq_len(n))[idx]
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(target in targets) {
      s <- split(t, function(v) v >= target, "sum")
    }
  })
  print(timing)
  timing
}

bench_split_custom_monoid_named <- function(n = 3000, queries = 200, use_cpp = TRUE) {
  message("== split custom monoid, named elements ==")
  sum_m <- MeasureMonoid(function(a, b) a + b, 0, function(el) as.numeric(el))
  vals <- as.list(seq_len(n))
  names(vals) <- paste0("k", seq_len(n))
  t <- tree_from(vals, monoids = list(sum = sum_m))
  idx <- sample.int(n, queries, replace = TRUE)
  targets <- cumsum(seq_len(n))[idx]
  timing <- system.time({
    options(fingertree.use_cpp = use_cpp)
    for(target in targets) {
      s <- split(t, function(v) v >= target, "sum")
    }
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
    #from_default = bench_tree_from_default(n = n, use_cpp = use_cpp),
    #from_custom = bench_tree_from_custom_monoid(n = n, use_cpp = use_cpp),
    #from_named = bench_tree_from_named(n = n, use_cpp = use_cpp),
    #from_named_custom = bench_tree_from_named_custom_monoid(n = n, use_cpp = use_cpp),
    # concat_default = bench_concat_default(n = max(100L, as.integer(n / 2L)), reps = 500, use_cpp = use_cpp),
    # concat_custom = bench_concat_custom_monoid(n = max(100L, as.integer(n / 2L)), reps = 500, use_cpp = use_cpp),
    # concat_named = bench_concat_named(n = max(100L, as.integer(n / 2L)), reps = 500, use_cpp = use_cpp),
    # concat_named_custom = bench_concat_named_custom_monoid(n = max(100L, as.integer(n / 2L)), reps = 500, use_cpp = use_cpp),
    locate_default = bench_locate_default(n = max(500L, as.integer(n)), queries = 200, use_cpp = use_cpp),
    locate_custom = bench_locate_custom_monoid(n = max(500L, as.integer(n)), queries = 200, use_cpp = use_cpp),
    locate_custom_named = bench_locate_custom_monoid_named(n = max(500L, as.integer(n)), queries = 200, use_cpp = use_cpp),
    split_tree_default = bench_split_tree_default(n = max(500L, as.integer(n)), queries = 200, use_cpp = use_cpp),
    split_tree_custom = bench_split_tree_custom_monoid(n = max(500L, as.integer(n)), queries = 200, use_cpp = use_cpp),
    split_tree_custom_named = bench_split_tree_custom_monoid_named(n = max(500L, as.integer(n)), queries = 200, use_cpp = use_cpp),
    split_default = bench_split_default(n = max(500L, as.integer(n)), queries = 200, use_cpp = use_cpp),
    split_custom = bench_split_custom_monoid(n = max(500L, as.integer(n)), queries = 200, use_cpp = use_cpp),
    split_custom_named = bench_split_custom_monoid_named(n = max(500L, as.integer(n)), queries = 200, use_cpp = use_cpp)
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
