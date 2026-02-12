# Stack-safety stress probe for replacement indexing.
# This isolates each replacement call in a subprocess so a stack overflow
# or segfault does not kill your interactive R session.
#
# Usage:
#   source("meta/stack_probe.R")
#   out <- probe_replacement_stack(
#     widths = seq(50, 2000, by = 50),
#     n = 2000,
#     named = FALSE,
#     use_cpp_runtime = FALSE
#   )
#   out

if(requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
}

.probe_one_replacement <- function(width, n, named, use_cpp_runtime, seed) {
  if(requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(quiet = TRUE)
  } else {
    library(fingertree)
  }

  set.seed(seed)

  # Always use C++ for setup so the probe isolates replacement behavior.
  options(fingertree.use_cpp = TRUE)
  if(named) {
    vals <- as.list(seq_len(n))
    names(vals) <- paste0("k", seq_len(n))
    t <- tree_from(vals)
    idx <- sample(names(vals), width, replace = TRUE)
    repl <- as.list(seq_len(width) + n)
  } else {
    t <- tree_from(as.list(seq_len(n)))
    idx <- sample.int(n, width, replace = TRUE)
    repl <- as.list(seq_len(width) + n)
  }

  options(fingertree.use_cpp = use_cpp_runtime)
  tm <- system.time({
    t[idx] <- repl
  })

  list(ok = TRUE, elapsed = unname(tm[["elapsed"]]))
}

.probe_case <- function(width, n, named, use_cpp_runtime, seed) {
  if(requireNamespace("callr", quietly = TRUE)) {
    res <- tryCatch(
      callr::r(
        function(width, n, named, use_cpp_runtime, seed) {
          if(requireNamespace("devtools", quietly = TRUE)) {
            devtools::load_all(quiet = TRUE)
          } else {
            library(fingertree)
          }

          set.seed(seed)
          options(fingertree.use_cpp = TRUE)
          if(isTRUE(named)) {
            vals <- as.list(seq_len(n))
            names(vals) <- paste0("k", seq_len(n))
            t <- tree_from(vals)
            idx <- sample(names(vals), width, replace = TRUE)
            repl <- as.list(seq_len(width) + n)
          } else {
            t <- tree_from(as.list(seq_len(n)))
            idx <- sample.int(n, width, replace = TRUE)
            repl <- as.list(seq_len(width) + n)
          }

          options(fingertree.use_cpp = isTRUE(use_cpp_runtime))
          tm <- system.time({
            t[idx] <- repl
          })
          list(ok = TRUE, elapsed = unname(tm[["elapsed"]]))
        },
        args = list(width, n, named, use_cpp_runtime, seed),
        spinner = FALSE,
        show = FALSE
      ),
      error = function(e) e
    )
  } else {
    warning("Package 'callr' not installed; running probe in-process.")
    res <- tryCatch(
      .probe_one_replacement(
        width = width,
        n = n,
        named = named,
        use_cpp_runtime = use_cpp_runtime,
        seed = seed
      ),
      error = function(e) e
    )
  }

  if(inherits(res, "error")) {
    return(list(ok = FALSE, elapsed = NA_real_, message = conditionMessage(res)))
  }
  list(ok = isTRUE(res$ok), elapsed = res$elapsed, message = NA_character_)
}

probe_replacement_stack <- function(
  widths = seq(200, 2000, by = 200),
  n = 2000,
  named = FALSE,
  use_cpp_runtime = FALSE,
  seed = 42
) {
  out <- data.frame(
    width = as.integer(widths),
    ok = NA,
    elapsed = NA_real_,
    message = NA_character_,
    stringsAsFactors = FALSE
  )

  for(i in seq_along(widths)) {
    w <- as.integer(widths[[i]])
    cat(sprintf("probe width=%d ... ", w))
    res <- .probe_case(
      width = w,
      n = n,
      named = named,
      use_cpp_runtime = use_cpp_runtime,
      seed = seed + i
    )
    out$ok[[i]] <- isTRUE(res$ok)
    out$elapsed[[i]] <- res$elapsed
    out$message[[i]] <- res$message
    if(isTRUE(res$ok)) {
      cat(sprintf("ok (%.3fs)\n", res$elapsed))
    } else {
      cat("FAILED\n")
    }
  }

  out
}



#source("meta/stack_probe.R")

# Integer replacement stress (R path)
out_int <- probe_replacement_stack(
  widths = seq(500, 2000, by = 500),
  n = 2000,
  named = FALSE,
  use_cpp_runtime = FALSE
)

# Name replacement stress (R path)
out_name <- probe_replacement_stack(
  widths = seq(500, 1000, by = 500),
  n = 2000,
  named = TRUE,
  use_cpp_runtime = FALSE
)
