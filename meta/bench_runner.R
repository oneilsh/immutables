# Lightweight benchmark runner with local result persistence.
# Intended for ad-hoc performance tracking outside testthat.
#
# Usage examples:
#   source("meta/bench_runner.R")
#   run_quick(note = "testing bulk merge", use_cpp = TRUE)
#   run_full(note = "before c++ insert changes", use_cpp = FALSE)
#   compare_last(n = 2)

.bench_now_utc <- function() {
  format(Sys.time(), "%Y-%m-%dT%H-%M-%SZ", tz = "UTC")
}

.bench_run_stamp <- function() {
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
}

.bench_git_value <- function(args, default = "unknown") {
  out <- tryCatch(
    system2("git", args = args, stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if(length(out) == 0L) {
    return(default)
  }
  val <- trimws(out[[1L]])
  if(!nzchar(val)) default else val
}

.bench_machine <- function() {
  info <- Sys.info()
  paste(info[["sysname"]], info[["release"]], info[["machine"]])
}

.bench_write_json <- function(x, path) {
  if(requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(x, path = path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    return(invisible(path))
  }
  fallback <- sub("\\.json$", ".dput", path)
  dput(x, file = fallback)
  warning("Package 'jsonlite' not available; wrote metadata to ", fallback)
  invisible(fallback)
}

.bench_ensure_load_all <- function(load_pkg = TRUE) {
  if(!isTRUE(load_pkg)) {
    return(invisible(NULL))
  }
  if(!requireNamespace("devtools", quietly = TRUE)) {
    stop("Package 'devtools' is required when load_pkg = TRUE.")
  }
  devtools::load_all(quiet = TRUE)
}

.bench_results_dir <- function(results_dir) {
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(results_dir, winslash = "/", mustWork = TRUE)
}

.bench_timing_row <- function(scenario, suite, use_cpp, params, expr) {
  gc(verbose = FALSE)
  timing <- system.time(force(expr))
  data.frame(
    scenario = scenario,
    suite = suite,
    use_cpp = isTRUE(use_cpp),
    params = paste(sprintf("%s=%s", names(params), unlist(params, use.names = FALSE)), collapse = ";"),
    user = as.numeric(timing[["user.self"]]),
    system = as.numeric(timing[["sys.self"]]),
    elapsed = as.numeric(timing[["elapsed"]]),
    stringsAsFactors = FALSE
  )
}

.bench_registry <- function() {
  list(
    flexseq_tree_from = list(required = c("as_flexseq")),
    flexseq_tree_from_named = list(required = c("as_flexseq")),
    flexseq_concat = list(required = c("as_flexseq", "c")),
    locate_default = list(required = c("as_flexseq", "locate_by_predicate")),
    split_tree_default = list(required = c("as_flexseq", "split_around_by_predicate")),
    split_default = list(required = c("as_flexseq", "split_by_predicate")),
    index_integer_single_read = list(required = c("as_flexseq")),
    index_name_single_read = list(required = c("as_flexseq")),
    as_flexseq_only = list(required = c("as_flexseq")),
    ordered_sequence_insert = list(required = c("as_ordered_sequence", "insert")),
    ordered_sequence_bounds = list(required = c("as_ordered_sequence", "lower_bound", "upper_bound")),
    ordered_sequence_range_queries = list(required = c("as_ordered_sequence", "count_between", "elements_between")),
    ordered_sequence_pop_cycle = list(required = c("as_ordered_sequence", "pop_key", "insert")),
    pq_peek_min_max = list(required = c("priority_queue", "insert", "peek_min", "peek_max")),
    pq_pop_min_drain = list(required = c("priority_queue", "insert", "pop_min")),
    pq_mixed_ops = list(required = c("priority_queue", "insert", "pop_min", "peek_min")),
    pq_insert_pop = list(required = c("priority_queue", "insert", "pop_min"))
  )
}

.bench_require_symbols <- function(scenario) {
  reg <- .bench_registry()
  cfg <- reg[[scenario]]
  if(is.null(cfg)) {
    stop("Unknown benchmark scenario: ", scenario)
  }
  missing <- cfg$required[!vapply(cfg$required, exists, logical(1), mode = "function", inherits = TRUE)]
  if(length(missing) > 0L) {
    stop(
      "Scenario '", scenario, "' is out of sync with current API. Missing function(s): ",
      paste(missing, collapse = ", "),
      "."
    )
  }
  invisible(TRUE)
}

.bench_sanity_check <- function(scenario) {
  if(identical(scenario, "flexseq_tree_from")) {
    x <- as_flexseq(as.list(1:3))
    if(!inherits(x, "flexseq")) stop("Scenario contract failed: as_flexseq() must return flexseq.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "flexseq_tree_from_named")) {
    x <- as.list(1:3)
    names(x) <- c("a", "b", "c")
    t <- as_flexseq(x)
    if(!inherits(t, "flexseq")) stop("Scenario contract failed: named as_flexseq() must return flexseq.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "flexseq_concat")) {
    a <- as_flexseq(as.list(1:2))
    b <- as_flexseq(as.list(3:4))
    z <- c(a, b)
    if(!inherits(z, "flexseq")) stop("Scenario contract failed: c(flexseq, flexseq) must return flexseq.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "locate_default")) {
    t <- as_flexseq(as.list(1:5))
    out <- locate_by_predicate(t, function(v) v >= 3, ".size")
    if(!is.list(out) || is.null(out$found)) stop("Scenario contract failed: locate_by_predicate output shape changed.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "split_tree_default")) {
    t <- as_flexseq(as.list(1:5))
    out <- split_around_by_predicate(t, function(v) v >= 3, ".size")
    if(!is.list(out) || is.null(out$left) || is.null(out$right)) stop("Scenario contract failed: split_around_by_predicate output shape changed.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "split_default")) {
    t <- as_flexseq(as.list(1:5))
    out <- split_by_predicate(t, function(v) v >= 3, ".size")
    if(!is.list(out) || is.null(out$left) || is.null(out$right)) stop("Scenario contract failed: split_by_predicate output shape changed.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "index_integer_single_read")) {
    t <- as_flexseq(as.list(1:3))
    v <- t[[2]]
    if(!identical(v, 2L) && !identical(v, 2)) stop("Scenario contract failed: integer [[ on flexseq changed.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "index_name_single_read")) {
    x <- as.list(1:3)
    names(x) <- c("a", "b", "c")
    t <- as_flexseq(x)
    v <- t[["b"]]
    if(!identical(v, 2L) && !identical(v, 2)) stop("Scenario contract failed: name [[ on flexseq changed.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "as_flexseq_only")) {
    x <- as_flexseq(as.list(1:3))
    if(!inherits(x, "flexseq")) stop("Scenario contract failed: as_flexseq_only requires flexseq return.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "ordered_sequence_insert")) {
    x <- as_ordered_sequence(list("b", "a"), keys = c(2, 1))
    y <- insert(x, "c", key = 2)
    if(!inherits(y, "ordered_sequence")) stop("Scenario contract failed: insert(ordered_sequence, ...) changed.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "ordered_sequence_bounds")) {
    x <- as_ordered_sequence(list("a", "b", "c"), keys = c(1, 2, 2))
    lb <- lower_bound(x, 2)
    ub <- upper_bound(x, 2)
    if(!is.list(lb) || is.null(lb$found) || !is.list(ub) || is.null(ub$found)) {
      stop("Scenario contract failed: lower_bound()/upper_bound() output shape changed.")
    }
    return(invisible(TRUE))
  }
  if(identical(scenario, "ordered_sequence_range_queries")) {
    x <- as_ordered_sequence(list("a", "b", "c"), keys = c(1, 2, 3))
    n <- count_between(x, 1, 2)
    ys <- elements_between(x, 1, 2)
    if(!is.integer(n) || !is.list(ys)) {
      stop("Scenario contract failed: range query helpers changed.")
    }
    return(invisible(TRUE))
  }
  if(identical(scenario, "ordered_sequence_pop_cycle")) {
    x <- as_ordered_sequence(list("a", "b", "c"), keys = c(1, 2, 3))
    out <- pop_key(x, 2, which = "first")
    if(!is.list(out) || is.null(out$sequence) || !inherits(out$sequence, "ordered_sequence")) {
      stop("Scenario contract failed: pop_key() output shape changed.")
    }
    y <- insert(out$sequence, out$element, key = 2)
    if(!inherits(y, "ordered_sequence")) {
      stop("Scenario contract failed: insert(pop_key(...)) changed.")
    }
    return(invisible(TRUE))
  }
  if(identical(scenario, "pq_peek_min_max")) {
    q <- priority_queue()
    q <- insert(q, "a", priority = 2)
    q <- insert(q, "b", priority = 1)
    a <- peek_min(q)
    b <- peek_max(q)
    if(is.null(a) || is.null(b)) {
      stop("Scenario contract failed: peek_min()/peek_max() returned NULL.")
    }
    return(invisible(TRUE))
  }
  if(identical(scenario, "pq_pop_min_drain")) {
    q <- priority_queue()
    q <- insert(q, "a", priority = 2)
    q <- insert(q, "b", priority = 1)
    out <- pop_min(q)
    if(!is.list(out) || is.null(out$queue) || !inherits(out$queue, "priority_queue")) {
      stop("Scenario contract failed: pop_min() output shape changed.")
    }
    return(invisible(TRUE))
  }
  if(identical(scenario, "pq_mixed_ops")) {
    q <- priority_queue()
    q <- insert(q, "a", priority = 2)
    q <- insert(q, "b", priority = 1)
    q <- pop_min(q)$queue
    if(length(q) > 0L) {
      invisible(peek_min(q))
    }
    if(!inherits(q, "priority_queue")) {
      stop("Scenario contract failed: priority queue mixed operations changed class.")
    }
    return(invisible(TRUE))
  }
  if(identical(scenario, "pq_insert_pop")) {
    q <- priority_queue()
    q <- insert(q, "a", priority = 2)
    q <- insert(q, "b", priority = 1)
    out <- pop_min(q)
    if(!is.list(out) || is.null(out$queue) || !inherits(out$queue, "priority_queue")) {
      stop("Scenario contract failed: pop_min() output shape changed.")
    }
    return(invisible(TRUE))
  }
  invisible(TRUE)
}

.bench_profile <- function(suite = c("quick", "full")) {
  suite <- match.arg(suite)
  if(identical(suite, "quick")) {
    return(list(
      flexseq_tree_from = list(n = 4000L),
      flexseq_tree_from_named = list(n = 3000L),
      flexseq_concat = list(n = 800L, reps = 8L),
      locate_default = list(n = 2000L, queries = 40L),
      split_tree_default = list(n = 2000L, queries = 30L),
      split_default = list(n = 2000L, queries = 30L),
      index_integer_single_read = list(n = 1000L, queries = 200L),
      index_name_single_read = list(n = 1000L, queries = 200L),
      as_flexseq_only = list(n = 40000L),
      ordered_sequence_insert = list(n = 300L, inserts = 40L, key_space = 250L),
      ordered_sequence_bounds = list(n = 160L, queries = 30L, key_space = 120L),
      ordered_sequence_range_queries = list(n = 160L, queries = 30L, key_space = 120L),
      ordered_sequence_pop_cycle = list(n = 120L, steps = 30L, key_space = 100L),
      pq_peek_min_max = list(n = 100L, key_space = 80L, queries = 60L),
      pq_pop_min_drain = list(n = 100L, key_space = 80L, pops = 50L),
      pq_mixed_ops = list(n = 90L, key_space = 70L, ops = 110L, insert_prob = 0.55),
      pq_insert_pop = list(n = 100L, key_space = 80L, pops = 50L)
    ))
  }
  list(
    flexseq_tree_from = list(n = 26000L),
    flexseq_tree_from_named = list(n = 23000L),
    flexseq_concat = list(n = 8000L, reps = 200L),
    locate_default = list(n = 10000L, queries = 200L),
    split_tree_default = list(n = 5000L, queries = 80L),
    split_default = list(n = 5000L, queries = 80L),
    index_integer_single_read = list(n = 5000L, queries = 800L),
    index_name_single_read = list(n = 320L, queries = 60L),
    as_flexseq_only = list(n = 12000L),
    ordered_sequence_insert = list(n = 500L, inserts = 70L, key_space = 400L),
    ordered_sequence_bounds = list(n = 220L, queries = 44L, key_space = 170L),
    ordered_sequence_range_queries = list(n = 220L, queries = 44L, key_space = 170L),
    ordered_sequence_pop_cycle = list(n = 160L, steps = 44L, key_space = 130L),
    pq_peek_min_max = list(n = 140L, key_space = 100L, queries = 80L),
    pq_pop_min_drain = list(n = 140L, key_space = 100L, pops = 70L),
    pq_mixed_ops = list(n = 120L, key_space = 90L, ops = 150L, insert_prob = 0.55),
    pq_insert_pop = list(n = 140L, key_space = 100L, pops = 70L)
  )
}

.bench_scenario_flexseq_tree_from <- function(n) {
  x <- as.list(seq_len(as.integer(n)))
  invisible(as_flexseq(x))
}

.bench_scenario_flexseq_tree_from_named <- function(n) {
  n <- as.integer(n)
  x <- as.list(seq_len(n))
  names(x) <- paste0("k", seq_len(n))
  invisible(as_flexseq(x))
}

.bench_scenario_flexseq_concat <- function(n, reps) {
  n <- as.integer(n)
  reps <- as.integer(reps)
  left <- as_flexseq(as.list(seq_len(n)))
  right <- as_flexseq(as.list(seq_len(n) + n))
  for(i in seq_len(reps)) {
    invisible(c(left, right))
  }
  invisible(NULL)
}

.bench_scenario_locate_default <- function(n, queries) {
  n <- as.integer(n)
  queries <- as.integer(queries)
  t <- as_flexseq(as.list(seq_len(n)))
  idx <- sample.int(n, queries, replace = TRUE)
  for(i in idx) {
    invisible(locate_by_predicate(t, function(v) v >= i, ".size"))
  }
  invisible(NULL)
}

.bench_scenario_split_tree_default <- function(n, queries) {
  n <- as.integer(n)
  queries <- as.integer(queries)
  t <- as_flexseq(as.list(seq_len(n)))
  idx <- sample.int(n, queries, replace = TRUE)
  for(i in idx) {
    invisible(split_around_by_predicate(t, function(v) v >= i, ".size"))
  }
  invisible(NULL)
}

.bench_scenario_split_default <- function(n, queries) {
  n <- as.integer(n)
  queries <- as.integer(queries)
  t <- as_flexseq(as.list(seq_len(n)))
  idx <- sample.int(n, queries, replace = TRUE)
  for(i in idx) {
    invisible(split_by_predicate(t, function(v) v >= i, ".size"))
  }
  invisible(NULL)
}

.bench_scenario_index_integer_single_read <- function(n, queries) {
  n <- as.integer(n)
  queries <- as.integer(queries)
  t <- as_flexseq(as.list(seq_len(n)))
  idx <- sample.int(n, queries, replace = TRUE)
  for(i in idx) {
    invisible(t[[i]])
  }
  invisible(NULL)
}

.bench_scenario_index_name_single_read <- function(n, queries) {
  n <- as.integer(n)
  queries <- as.integer(queries)
  vals <- as.list(seq_len(n))
  names(vals) <- paste0("k", seq_len(n))
  t <- as_flexseq(vals)
  idx <- sample(names(vals), queries, replace = TRUE)
  for(nm in idx) {
    invisible(t[[nm]])
  }
  invisible(NULL)
}

.bench_scenario_as_flexseq_only <- function(n) {
  n <- as.integer(n)
  x <- as.list(seq_len(n))
  invisible(as_flexseq(x))
}

.bench_scenario_ordered_sequence_insert <- function(n, inserts, key_space) {
  n <- as.integer(n)
  inserts <- as.integer(inserts)
  key_space <- as.integer(key_space)

  base_vals <- as.list(seq_len(n))
  base_keys <- sample.int(key_space, n, replace = TRUE)
  seq <- as_ordered_sequence(base_vals, keys = base_keys)

  ins_keys <- sample.int(key_space, inserts, replace = TRUE)
  for(i in seq_len(inserts)) {
    seq <- insert(seq, i, key = ins_keys[[i]])
  }
  invisible(seq)
}

.bench_scenario_ordered_sequence_bounds <- function(n, queries, key_space) {
  n <- as.integer(n)
  queries <- as.integer(queries)
  key_space <- as.integer(key_space)

  vals <- as.list(seq_len(n))
  keys <- sample.int(key_space, n, replace = TRUE)
  seq <- as_ordered_sequence(vals, keys = keys)

  q <- sample.int(key_space, queries, replace = TRUE)
  for(k in q) {
    invisible(lower_bound(seq, k))
    invisible(upper_bound(seq, k))
  }
  invisible(NULL)
}

.bench_scenario_ordered_sequence_range_queries <- function(n, queries, key_space) {
  n <- as.integer(n)
  queries <- as.integer(queries)
  key_space <- as.integer(key_space)

  vals <- as.list(seq_len(n))
  keys <- sample.int(key_space, n, replace = TRUE)
  seq <- as_ordered_sequence(vals, keys = keys)

  lo <- sample.int(key_space, queries, replace = TRUE)
  hi <- sample.int(key_space, queries, replace = TRUE)
  for(i in seq_len(queries)) {
    a <- min(lo[[i]], hi[[i]])
    b <- max(lo[[i]], hi[[i]])
    invisible(count_between(seq, a, b))
    invisible(elements_between(seq, a, b))
  }
  invisible(NULL)
}

.bench_scenario_ordered_sequence_pop_cycle <- function(n, steps, key_space) {
  n <- as.integer(n)
  steps <- as.integer(steps)
  key_space <- as.integer(key_space)

  vals <- as.list(seq_len(n))
  keys <- sample.int(key_space, n, replace = TRUE)
  seq <- as_ordered_sequence(vals, keys = keys)

  q <- sample.int(key_space, steps, replace = TRUE)
  for(k in q) {
    out <- pop_key(seq, k, which = "first")
    if(!is.null(out$element)) {
      seq <- insert(out$sequence, out$element, key = as.integer(k))
    } else {
      seq <- out$sequence
    }
  }
  invisible(seq)
}

.bench_build_priority_queue <- function(n, key_space) {
  n <- as.integer(n)
  key_space <- as.integer(key_space)
  priorities <- sample.int(key_space, n, replace = TRUE)
  q <- priority_queue()
  for(i in seq_len(n)) {
    q <- insert(q, i, priority = priorities[[i]])
  }
  q
}

.bench_scenario_pq_insert_pop <- function(n, key_space, pops) {
  pops <- as.integer(pops)
  q <- .bench_build_priority_queue(n, key_space)
  k <- min(pops, n)
  for(i in seq_len(k)) {
    q <- pop_min(q)$queue
  }
  invisible(q)
}

.bench_scenario_pq_peek_min_max <- function(n, key_space, queries) {
  queries <- as.integer(queries)
  q <- .bench_build_priority_queue(n, key_space)
  for(i in seq_len(queries)) {
    invisible(peek_min(q))
    invisible(peek_max(q))
  }
  invisible(NULL)
}

.bench_scenario_pq_pop_min_drain <- function(n, key_space, pops) {
  pops <- as.integer(pops)
  q <- .bench_build_priority_queue(n, key_space)
  k <- min(pops, as.integer(n))
  for(i in seq_len(k)) {
    q <- pop_min(q)$queue
    if(length(q) == 0L) {
      break
    }
  }
  invisible(q)
}

.bench_scenario_pq_mixed_ops <- function(n, key_space, ops, insert_prob = 0.55) {
  n <- as.integer(n)
  ops <- as.integer(ops)
  key_space <- as.integer(key_space)
  insert_prob <- as.numeric(insert_prob)
  q <- .bench_build_priority_queue(n, key_space)
  next_id <- n
  for(i in seq_len(ops)) {
    do_insert <- (length(q) == 0L) || (runif(1) < insert_prob)
    if(do_insert) {
      next_id <- next_id + 1L
      q <- insert(q, next_id, priority = sample.int(key_space, 1L))
    } else {
      q <- pop_min(q)$queue
      if(length(q) > 0L) {
        invisible(peek_min(q))
      }
    }
  }
  invisible(q)
}

.bench_known_scenarios <- function() {
  c(
    "flexseq_tree_from",
    "flexseq_tree_from_named",
    "flexseq_concat",
    "locate_default",
    "split_tree_default",
    "split_default",
    "index_integer_single_read",
    "index_name_single_read",
    "as_flexseq_only",
    "ordered_sequence_insert",
    "ordered_sequence_bounds",
    "ordered_sequence_range_queries",
    "ordered_sequence_pop_cycle",
    "pq_peek_min_max",
    "pq_pop_min_drain",
    "pq_mixed_ops",
    "pq_insert_pop"
  )
}

.bench_run_scenario <- function(name, params) {
  .bench_require_symbols(name)
  .bench_sanity_check(name)
  switch(
    name,
    flexseq_tree_from = do.call(.bench_scenario_flexseq_tree_from, params),
    flexseq_tree_from_named = do.call(.bench_scenario_flexseq_tree_from_named, params),
    flexseq_concat = do.call(.bench_scenario_flexseq_concat, params),
    locate_default = do.call(.bench_scenario_locate_default, params),
    split_tree_default = do.call(.bench_scenario_split_tree_default, params),
    split_default = do.call(.bench_scenario_split_default, params),
    index_integer_single_read = do.call(.bench_scenario_index_integer_single_read, params),
    index_name_single_read = do.call(.bench_scenario_index_name_single_read, params),
    as_flexseq_only = do.call(.bench_scenario_as_flexseq_only, params),
    ordered_sequence_insert = do.call(.bench_scenario_ordered_sequence_insert, params),
    ordered_sequence_bounds = do.call(.bench_scenario_ordered_sequence_bounds, params),
    ordered_sequence_range_queries = do.call(.bench_scenario_ordered_sequence_range_queries, params),
    ordered_sequence_pop_cycle = do.call(.bench_scenario_ordered_sequence_pop_cycle, params),
    pq_peek_min_max = do.call(.bench_scenario_pq_peek_min_max, params),
    pq_pop_min_drain = do.call(.bench_scenario_pq_pop_min_drain, params),
    pq_mixed_ops = do.call(.bench_scenario_pq_mixed_ops, params),
    pq_insert_pop = do.call(.bench_scenario_pq_insert_pop, params),
    stop("Unknown benchmark scenario: ", name)
  )
}

.bench_resolve_custom_profile <- function(
  scenarios,
  base = c("quick", "full"),
  params = list()
) {
  base <- match.arg(base)
  default_profile <- .bench_profile(base)
  known <- .bench_known_scenarios()

  if(is.null(scenarios) || length(scenarios) == 0L) {
    stop("`scenarios` must include at least one scenario name.")
  }
  scenarios <- unique(as.character(scenarios))
  bad <- setdiff(scenarios, known)
  if(length(bad) > 0L) {
    stop("Unknown scenario(s): ", paste(bad, collapse = ", "))
  }

  profile <- vector("list", length(scenarios))
  names(profile) <- scenarios
  for(nm in scenarios) {
    base_params <- default_profile[[nm]]
    if(is.null(base_params)) {
      base_params <- list()
    }
    override <- params[[nm]]
    if(!is.null(override)) {
      if(!is.list(override)) {
        stop("`params[['", nm, "']]` must be a list of parameter overrides.")
      }
      base_params[names(override)] <- override
    }
    profile[[nm]] <- base_params
  }
  profile
}

.run_bench_suite <- function(
  suite = c("quick", "full", "custom"),
  profile = NULL,
  note = "",
  use_cpp = TRUE,
  seed = 1L,
  load_pkg = TRUE,
  results_dir = "meta/bench-results"
) {
  suite <- match.arg(suite)
  .bench_ensure_load_all(load_pkg = load_pkg)
  results_dir <- .bench_results_dir(results_dir)

  old_cpp <- getOption("immutables.use_cpp")
  on.exit({
    if(is.null(old_cpp)) {
      options(immutables.use_cpp = NULL)
    } else {
      options(immutables.use_cpp = old_cpp)
    }
  }, add = TRUE)

  set.seed(as.integer(seed))
  options(immutables.use_cpp = isTRUE(use_cpp))

  run_id <- paste0(.bench_run_stamp(), "-", suite, "-", if(isTRUE(use_cpp)) "cpp" else "r")
  if(is.null(profile)) {
    if(identical(suite, "custom")) {
      stop("`profile` is required when suite = 'custom'.")
    }
    profile <- .bench_profile(suite)
  }
  rows <- list()

  add_row <- function(name, params, expr) {
    message(sprintf("[bench:%s] %s", suite, name))
    row <- .bench_timing_row(name, suite, use_cpp, params, expr)
    rows[[length(rows) + 1L]] <<- row
    message(sprintf("[bench:%s] %s elapsed=%.3fs", suite, name, row$elapsed[[1L]]))
  }

  for(name in names(profile)) {
    params <- profile[[name]]
    add_row(name, params, {
      .bench_run_scenario(name, params)
    })
  }

  out <- do.call(rbind, rows)
  out$run_id <- run_id
  out <- out[, c("run_id", "scenario", "suite", "use_cpp", "params", "user", "system", "elapsed")]

  csv_path <- file.path(results_dir, paste0(run_id, ".csv"))
  meta_path <- file.path(results_dir, paste0(run_id, ".meta.json"))
  utils::write.csv(out, file = csv_path, row.names = FALSE)

  meta <- list(
    note = as.character(note),
    git_sha = .bench_git_value(c("rev-parse", "--short", "HEAD")),
    branch = .bench_git_value(c("rev-parse", "--abbrev-ref", "HEAD")),
    datetime_utc = .bench_now_utc(),
    machine = .bench_machine(),
    use_cpp = isTRUE(use_cpp),
    suite = suite,
    env = list(
      immutables.use_cpp = as.character(getOption("immutables.use_cpp")),
      immutables.oms.merge_engine = as.character(getOption("immutables.oms.merge_engine")),
      IMMUTABLES_GC_STRESS = Sys.getenv("IMMUTABLES_GC_STRESS", unset = "")
    ),
    results_file = basename(csv_path)
  )
  .bench_write_json(meta, meta_path)

  message("Wrote: ", csv_path)
  message("Wrote: ", meta_path)
  print(out[, c("scenario", "elapsed")], row.names = FALSE)

  invisible(list(
    run_id = run_id,
    csv = csv_path,
    meta = meta_path,
    timings = out,
    metadata = meta
  ))
}

# Public helpers
run_quick <- function(note = "", use_cpp = TRUE, seed = 1L, load_pkg = TRUE, results_dir = "meta/bench-results") {
  .run_bench_suite(
    suite = "quick",
    note = note,
    use_cpp = use_cpp,
    seed = seed,
    load_pkg = load_pkg,
    results_dir = results_dir
  )
}

run_full <- function(note = "", use_cpp = TRUE, seed = 1L, load_pkg = TRUE, results_dir = "meta/bench-results") {
  .run_bench_suite(
    suite = "full",
    note = note,
    use_cpp = use_cpp,
    seed = seed,
    load_pkg = load_pkg,
    results_dir = results_dir
  )
}

list_scenarios <- function(base = c("quick", "full")) {
  base <- match.arg(base)
  default_profile <- .bench_profile(base)
  known <- .bench_known_scenarios()
  rows <- lapply(known, function(nm) {
    p <- default_profile[[nm]]
    data.frame(
      scenario = nm,
      in_base = !is.null(p),
      default_params = if(is.null(p) || length(p) == 0L) "" else paste(sprintf("%s=%s", names(p), unlist(p, use.names = FALSE)), collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  print(out, row.names = FALSE)
  invisible(out)
}

run_scenarios <- function(
  scenarios,
  note = "",
  use_cpp = TRUE,
  seed = 1L,
  load_pkg = TRUE,
  results_dir = "meta/bench-results",
  base = c("quick", "full"),
  params = list()
) {
  base <- match.arg(base)
  profile <- .bench_resolve_custom_profile(
    scenarios = scenarios,
    base = base,
    params = params
  )
  .run_bench_suite(
    suite = "custom",
    profile = profile,
    note = note,
    use_cpp = use_cpp,
    seed = seed,
    load_pkg = load_pkg,
    results_dir = results_dir
  )
}

compare_last <- function(n = 2L, results_dir = "meta/bench-results") {
  n <- as.integer(n)
  if(is.na(n) || n < 2L) {
    stop("`n` must be an integer >= 2.")
  }
  files <- sort(list.files(results_dir, pattern = "\\.csv$", full.names = TRUE))
  if(length(files) < n) {
    stop("Need at least ", n, " benchmark CSV files in ", results_dir, ".")
  }

  picked <- tail(files, n)
  old_file <- picked[[1L]]
  new_file <- picked[[length(picked)]]

  old <- utils::read.csv(old_file, stringsAsFactors = FALSE)
  new <- utils::read.csv(new_file, stringsAsFactors = FALSE)
  keep <- c("scenario", "elapsed")

  cmp <- merge(
    old[, keep, drop = FALSE],
    new[, keep, drop = FALSE],
    by = "scenario",
    all = TRUE,
    suffixes = c("_old", "_new")
  )
  cmp$delta_sec <- cmp$elapsed_new - cmp$elapsed_old
  cmp$delta_pct <- ifelse(
    is.na(cmp$elapsed_old) | cmp$elapsed_old == 0,
    NA_real_,
    100 * cmp$delta_sec / cmp$elapsed_old
  )
  cmp <- cmp[order(abs(cmp$delta_sec), decreasing = TRUE), ]
  rownames(cmp) <- NULL

  attr(cmp, "old_run") <- tools::file_path_sans_ext(basename(old_file))
  attr(cmp, "new_run") <- tools::file_path_sans_ext(basename(new_file))

  message("Comparing runs:")
  message("  old: ", attr(cmp, "old_run"))
  message("  new: ", attr(cmp, "new_run"))
  print(cmp, row.names = FALSE)
  invisible(cmp)
}
