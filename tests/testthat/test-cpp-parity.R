with_cpp_mode <- function(flag, expr) {
  old <- getOption("fingertree.use_cpp")
  options(fingertree.use_cpp = flag)
  on.exit({
    if(is.null(old)) {
      options(fingertree.use_cpp = NULL)
    } else {
      options(fingertree.use_cpp = old)
    }
  }, add = TRUE)
  force(expr)
}

backend_eval <- function(expr, flag, env = parent.frame()) {
  with_cpp_mode(flag, eval(expr, env))
}

expect_backend_identical <- function(expr, transform = identity, env = parent.frame()) {
  q <- substitute(expr)
  cpp <- transform(backend_eval(q, TRUE, env = env))
  r <- transform(backend_eval(q, FALSE, env = env))
  if(!identical(cpp, r)) {
    diff <- capture.output(all.equal(cpp, r))
    stop(
      paste0(
        "Backend parity mismatch for expression:\n",
        deparse(q),
        "\nDiff:\n",
        paste(diff, collapse = "\n")
      )
    )
  }
  testthat::succeed()
}

snapshot_tree <- function(t) {
  els <- fingertree:::.ft_to_list(t)
  vals <- lapply(els, fingertree:::.ft_strip_name)
  nms <- vapply(els, function(el) {
    nm <- fingertree:::.ft_get_name(el)
    if(is.null(nm)) "" else nm
  }, character(1))
  list(
    class = class(t),
    values = vals,
    names = nms,
    measures = attr(t, "measures", exact = TRUE),
    monoid_names = names(attr(t, "monoids", exact = TRUE))
  )
}

snapshot_split <- function(s) {
  list(
    left = snapshot_tree(s$left),
    elem = fingertree:::.ft_strip_name(s$elem),
    right = snapshot_tree(s$right)
  )
}

snapshot_locate <- function(x) {
  list(
    found = x$found,
    elem = if(isTRUE(x$found)) fingertree:::.ft_strip_name(x$elem) else NULL,
    index = x$index,
    left_measure = x$left_measure,
    hit_measure = x$hit_measure,
    right_measure = x$right_measure
  )
}

testthat::test_that("backend parity: MeasureMonoid constructor/use", {
  expect_backend_identical({
    m <- MeasureMonoid(function(a, b) a + b, 0, as.numeric)
    t <- tree_from(1:5)
    reduce_left(t, m)
  })
})

testthat::test_that("backend parity: Predicate constructor/use", {
  expect_backend_identical({
    p <- Predicate(function(v) v >= 3)
    t <- tree_from(letters[1:6])
    loc <- locate(t, p, ".size", include_metadata = TRUE)
    snapshot_locate(loc)
  })
})

testthat::test_that("backend parity: empty_tree", {
  expect_backend_identical({
    snapshot_tree(empty_tree())
  })
})

testthat::test_that("backend parity: tree_from", {
  expect_backend_identical({
    ms <- list(sum = MeasureMonoid(function(a, b) a + b, 0, as.numeric))
    snapshot_tree(tree_from(setNames(as.list(1:8), letters[1:8]), monoids = ms))
  })
})

testthat::test_that("backend parity: add_monoids", {
  expect_backend_identical({
    t <- tree_from(1:8)
    m <- MeasureMonoid(function(a, b) a + b, 0, as.numeric)
    snapshot_tree(add_monoids(t, list(sum = m)))
  })
})

testthat::test_that("backend parity: append", {
  expect_backend_identical({
    t <- tree_from(1:10)
    t <- append(t, 11)
    snapshot_tree(t)
  })
})

testthat::test_that("backend parity: prepend", {
  expect_backend_identical({
    t <- tree_from(1:10)
    t <- prepend(t, 0)
    snapshot_tree(t)
  })
})

testthat::test_that("backend parity: concat_trees", {
  expect_backend_identical({
    t1 <- tree_from(1:10)
    t2 <- tree_from(11:20)
    snapshot_tree(concat_trees(t1, t2))
  })
})

testthat::test_that("backend parity: reduce_left", {
  expect_backend_identical({
    t <- tree_from(1:25)
    m <- MeasureMonoid(function(a, b) a + b, 0, as.numeric)
    reduce_left(t, m)
  })
})

testthat::test_that("backend parity: reduce_right", {
  expect_backend_identical({
    t <- tree_from(1:25)
    m <- MeasureMonoid(function(a, b) a + b, 0, as.numeric)
    reduce_right(t, m)
  })
})

testthat::test_that("backend parity: split_tree", {
  expect_backend_identical({
    t <- tree_from(letters[1:20])
    snapshot_split(split_tree(t, function(v) v >= 8, ".size"))
  })
})

testthat::test_that("backend parity: split", {
  expect_backend_identical({
    t <- tree_from(letters[1:20])
    s <- split(t, function(v) v >= 8, ".size")
    list(left = snapshot_tree(s$left), right = snapshot_tree(s$right))
  })
})

testthat::test_that("backend parity: locate", {
  expect_backend_identical({
    t <- tree_from(letters[1:20])
    snapshot_locate(locate(t, function(v) v >= 10, ".size", include_metadata = TRUE))
  })
})

testthat::test_that("backend parity: [ read (integer/logical/name)", {
  expect_backend_identical({
    t <- tree_from(setNames(as.list(letters[1:12]), LETTERS[1:12]))
    list(
      int = snapshot_tree(t[c(2, 8, 8, 1)]),
      lgl = snapshot_tree(t[c(TRUE, FALSE, TRUE)]),
      chr = snapshot_tree(t[c("H", "A", "missing", "C")])
    )
  })
})

testthat::test_that("backend parity: [[ read (integer/name)", {
  expect_backend_identical({
    t <- tree_from(setNames(as.list(letters[1:12]), LETTERS[1:12]))
    list(int = t[[3]], chr = t[["C"]])
  })
})

testthat::test_that("backend parity: [<- replacement (integer/logical/name)", {
  expect_backend_identical({
    t1 <- tree_from(1:12)
    t1[c(2, 5, 5)] <- list(200, 500, 501)
    t1[c(TRUE, FALSE, TRUE)] <- list(9, 8)

    t2 <- tree_from(setNames(as.list(letters[1:8]), LETTERS[1:8]))
    t2[c("B", "D")] <- list("bb", "dd")

    list(int_lgl = snapshot_tree(t1), chr = snapshot_tree(t2))
  })
})

testthat::test_that("backend parity: [[<- replacement and deletion", {
  expect_backend_identical({
    t1 <- tree_from(1:10)
    t1[[4]] <- 99
    t1[[6]] <- NULL

    t2 <- tree_from(setNames(as.list(letters[1:8]), LETTERS[1:8]))
    t2[["C"]] <- structure("cc", names = "Z")
    t2[["E"]] <- NULL

    list(int = snapshot_tree(t1), chr = snapshot_tree(t2))
  })
})

testthat::test_that("backend parity: $ read", {
  expect_backend_identical({
    t <- tree_from(setNames(as.list(letters[1:8]), LETTERS[1:8]))
    list(A = t$A, C = t$C)
  })
})

testthat::test_that("backend parity: $<- replacement", {
  expect_backend_identical({
    t <- tree_from(setNames(as.list(letters[1:8]), LETTERS[1:8]))
    t$B <- "bbb"
    snapshot_tree(t)
  })
})

testthat::test_that("backend parity: print.FingerTree output", {
  expect_backend_identical({
    t <- tree_from(setNames(as.list(1:10), letters[1:10]))
    capture.output(print(t))
  })
})

testthat::test_that("backend parity: get_graph_df", {
  expect_backend_identical({
    t <- tree_from(letters[1:12])
    get_graph_df(t)
  })
})

testthat::test_that("backend parity: validate_tree", {
  expect_backend_identical({
    t <- tree_from(letters[1:10])
    isTRUE(validate_tree(t))
  })
})

testthat::test_that("backend parity: validate_name_state", {
  expect_backend_identical({
    t <- tree_from(setNames(as.list(letters[1:10]), LETTERS[1:10]))
    isTRUE(validate_name_state(t))
  })
})
