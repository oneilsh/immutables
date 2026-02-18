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
    oms_build = list(required = c("as_ordered_multiset")),
    oms_union = list(required = c("as_ordered_multiset", "union")),
    oms_intersection = list(required = c("as_ordered_multiset", "intersect")),
    oms_difference = list(required = c("as_ordered_multiset", "setdiff")),
    oms_insert = list(required = c("as_ordered_multiset", "insert")),
    as_flexseq_only = list(required = c("as_flexseq")),
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
  if(identical(scenario, "oms_build")) {
    ms <- as_ordered_multiset(list(2, 1), keys = list(2, 1))
    if(!inherits(ms, "ordered_multiset")) stop("Scenario contract failed: as_ordered_multiset class changed.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "oms_union")) {
    x <- as_ordered_multiset(list(2, 1), keys = list(2, 1))
    y <- as_ordered_multiset(list(2, 3), keys = list(2, 3))
    z <- union(x, y)
    if(!inherits(z, "ordered_multiset")) stop("Scenario contract failed: union(ordered_multiset, ...) changed.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "oms_intersection")) {
    x <- as_ordered_multiset(list(2, 1), keys = list(2, 1))
    y <- as_ordered_multiset(list(2, 3), keys = list(2, 3))
    z <- intersect(x, y)
    if(!inherits(z, "ordered_multiset")) stop("Scenario contract failed: intersect(ordered_multiset, ...) changed.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "oms_difference")) {
    x <- as_ordered_multiset(list(2, 1), keys = list(2, 1))
    y <- as_ordered_multiset(list(2, 3), keys = list(2, 3))
    z <- setdiff(x, y)
    if(!inherits(z, "ordered_multiset")) stop("Scenario contract failed: setdiff(ordered_multiset, ...) changed.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "oms_insert")) {
    x <- as_ordered_multiset(list(2, 1), keys = list(2, 1))
    y <- insert(x, 4, key = 4)
    if(!inherits(y, "ordered_multiset")) stop("Scenario contract failed: insert(ordered_multiset, ...) changed.")
    return(invisible(TRUE))
  }
  if(identical(scenario, "as_flexseq_only")) {
    x <- as_flexseq(as.list(1:3))
    if(!inherits(x, "flexseq")) stop("Scenario contract failed: as_flexseq_only requires flexseq return.")
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
      oms_build = list(n = 1500L, key_space = 1000L),
      oms_union = list(n = 1200L, reps = 4L, key_space = 1000L),
      oms_intersection = list(n = 1200L, reps = 4L, key_space = 1000L),
      oms_difference = list(n = 1200L, reps = 4L, key_space = 1000L),
      oms_insert = list(n = 1000L, inserts = 80L, key_space = 1000L),
      as_flexseq_only = list(n = 40000L)
    ))
  }
  list(
    flexseq_tree_from = list(n = 40000L),
    flexseq_tree_from_named = list(n = 30000L),
    flexseq_concat = list(n = 8000L, reps = 200L),
    locate_default = list(n = 10000L, queries = 200L),
    split_tree_default = list(n = 10000L, queries = 160L),
    split_default = list(n = 10000L, queries = 160L),
    index_integer_single_read = list(n = 10000L, queries = 2000L),
    index_name_single_read = list(n = 6000L, queries = 1200L),
    oms_build = list(n = 20000L, key_space = 2000L),
    oms_union = list(n = 12000L, reps = 120L, key_space = 2000L),
    oms_intersection = list(n = 12000L, reps = 120L, key_space = 2000L),
    oms_difference = list(n = 12000L, reps = 120L, key_space = 2000L),
    oms_insert = list(n = 12000L, inserts = 2500L, key_space = 2000L),
    as_flexseq_only = list(n = 120000L)
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

.bench_oms_random_keys <- function(n, key_space) {
  as.list(sample.int(as.integer(key_space), as.integer(n), replace = TRUE))
}

.bench_scenario_oms_build <- function(n, key_space) {
  vals <- .bench_oms_random_keys(n, key_space)
  invisible(as_ordered_multiset(vals, keys = vals))
}

.bench_scenario_oms_union <- function(n, reps, key_space) {
  reps <- as.integer(reps)
  x_vals <- .bench_oms_random_keys(n, key_space)
  y_vals <- .bench_oms_random_keys(n, key_space)
  x <- as_ordered_multiset(x_vals, keys = x_vals)
  y <- as_ordered_multiset(y_vals, keys = y_vals)
  for(i in seq_len(reps)) {
    invisible(union(x, y))
  }
  invisible(NULL)
}

.bench_scenario_oms_intersection <- function(n, reps, key_space) {
  reps <- as.integer(reps)
  x_vals <- .bench_oms_random_keys(n, key_space)
  y_vals <- .bench_oms_random_keys(n, key_space)
  x <- as_ordered_multiset(x_vals, keys = x_vals)
  y <- as_ordered_multiset(y_vals, keys = y_vals)
  for(i in seq_len(reps)) {
    invisible(intersect(x, y))
  }
  invisible(NULL)
}

.bench_scenario_oms_difference <- function(n, reps, key_space) {
  reps <- as.integer(reps)
  x_vals <- .bench_oms_random_keys(n, key_space)
  y_vals <- .bench_oms_random_keys(n, key_space)
  x <- as_ordered_multiset(x_vals, keys = x_vals)
  y <- as_ordered_multiset(y_vals, keys = y_vals)
  for(i in seq_len(reps)) {
    invisible(setdiff(x, y))
  }
  invisible(NULL)
}

.bench_scenario_oms_insert <- function(n, inserts, key_space) {
  inserts <- as.integer(inserts)
  base_vals <- .bench_oms_random_keys(n, key_space)
  ins_keys <- sample.int(as.integer(key_space), inserts, replace = TRUE)
  ms <- as_ordered_multiset(base_vals, keys = base_vals)
  for(i in seq_len(inserts)) {
    k <- as.integer(ins_keys[[i]])
    ms <- insert(ms, k, key = k)
  }
  invisible(ms)
}

.bench_scenario_as_flexseq_only <- function(n) {
  n <- as.integer(n)
  x <- as.list(seq_len(n))
  invisible(as_flexseq(x))
}

.bench_scenario_pq_insert_pop <- function(n, key_space, pops) {
  n <- as.integer(n)
  pops <- as.integer(pops)
  priorities <- sample.int(as.integer(key_space), n, replace = TRUE)
  q <- priority_queue()
  for(i in seq_len(n)) {
    q <- insert(q, i, priority = priorities[[i]])
  }
  k <- min(pops, n)
  for(i in seq_len(k)) {
    q <- pop_min(q)$queue
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
    "oms_build",
    "oms_union",
    "oms_intersection",
    "oms_difference",
    "oms_insert",
    "as_flexseq_only",
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
    oms_build = do.call(.bench_scenario_oms_build, params),
    oms_union = do.call(.bench_scenario_oms_union, params),
    oms_intersection = do.call(.bench_scenario_oms_intersection, params),
    oms_difference = do.call(.bench_scenario_oms_difference, params),
    oms_insert = do.call(.bench_scenario_oms_insert, params),
    as_flexseq_only = do.call(.bench_scenario_as_flexseq_only, params),
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
