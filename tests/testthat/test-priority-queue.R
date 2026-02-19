testthat::test_that("priority_queue constructor and length", {
  q0 <- priority_queue()
  testthat::expect_true(inherits(q0, "priority_queue"))
  testthat::expect_true(inherits(q0, "flexseq"))
  testthat::expect_identical(length(q0), 0L)

  q <- priority_queue("a", "b", "c", priorities = c(3, 1, 2))
  testthat::expect_identical(length(q), 3L)
  testthat::expect_equal(peek_min(q), "b")
  testthat::expect_equal(peek_max(q), "a")
})

testthat::test_that("min/max pop uses sequence order on ties", {
  q <- priority_queue("a", "b", "c", "d", priorities = c(2, 1, 1, 2))

  e1 <- pop_min(q)
  testthat::expect_equal(e1$element, "b")
  testthat::expect_equal(e1$priority, 1)

  e2 <- pop_min(e1$remaining)
  testthat::expect_equal(e2$element, "c")
  testthat::expect_equal(e2$priority, 1)

  x1 <- pop_max(q)
  testthat::expect_equal(x1$element, "a")
  testthat::expect_equal(x1$priority, 2)

  x2 <- pop_max(x1$remaining)
  testthat::expect_equal(x2$element, "d")
  testthat::expect_equal(x2$priority, 2)
})

testthat::test_that("insert is persistent and supports names", {
  q <- as_priority_queue(setNames(as.list(c("x", "y")), c("kx", "ky")), priorities = c(5, 1))
  q2 <- insert(q, "z", priority = 1, name = "kz")

  testthat::expect_equal(peek_min(q), "y")
  testthat::expect_equal(peek_min(q2), "y")
  testthat::expect_equal(q2[["kz"]]$item, "z")
  testthat::expect_equal(length(q2), 3L)
})

testthat::test_that("priority_queue disallows sequence-style mutation helpers", {
  q <- priority_queue("b", "c", priorities = c(2, 3))
  testthat::expect_error(push_front(q, list(item = "a", priority = 1)), "Cast first")
  testthat::expect_error(push_back(q, list(item = "d", priority = 4)), "Cast first")
  testthat::expect_error(peek_front(q), "not supported for priority_queue")
  testthat::expect_error(peek_back(q), "not supported for priority_queue")
  testthat::expect_error(pop_front(q), "not supported for priority_queue")
  testthat::expect_error(pop_back(q), "not supported for priority_queue")
  testthat::expect_error(c(q, q), "Cast first")
})

testthat::test_that("priority_queue character subset is strict on missing names", {
  q <- as_priority_queue(
    setNames(as.list(c("a", "b", "c")), c("ka", "kb", "kc")),
    priorities = c(3, 1, 2)
  )
  s <- q[c("kb", "ka")]

  testthat::expect_s3_class(s, "priority_queue")
  testthat::expect_equal(peek_min(s), "b")
  testthat::expect_error(q[c("kb", "missing")], "Unknown element name")
})

testthat::test_that("priority_queue disallows positional/logical indexing and replacements", {
  q <- priority_queue("a", "b", priorities = c(2, 1))

  testthat::expect_error(q[[1]], "scalar character names only")
  testthat::expect_error(q[1:2], "character name indexing only")
  testthat::expect_error(q[c(TRUE, FALSE)], "character name indexing only")
  testthat::expect_error({ q[[1]] <- list(item = "x", priority = 5) }, "Cast first")
  testthat::expect_error({ q[c(1, 2)] <- list("u", "v") }, "Cast first")
})

testthat::test_that("priority queue carries required monoids", {
  q <- priority_queue(1, 2, priorities = c(10, 20))
  ms <- names(attr(q, "monoids", exact = TRUE))
  testthat::expect_true(all(c(".size", ".named_count", ".pq_min", ".pq_max") %in% ms))
})

testthat::test_that("values path is removed", {
  testthat::expect_error(as_flexseq(1:3, values = 1:3), "unused argument")
  testthat::expect_error(tree_from(1:3, values = 1:3), "unused argument")
})

testthat::test_that("priority_queue casts down to flexseq explicitly", {
  q_named <- as_priority_queue(setNames(as.list(c("x", "y")), c("kx", "ky")), priorities = c(2, 1))
  x <- as_flexseq(q_named)

  testthat::expect_s3_class(x, "flexseq")
  testthat::expect_false(inherits(x, "priority_queue"))
  testthat::expect_true(all(c(".size", ".named_count") %in% names(attr(x, "monoids", exact = TRUE))))
  testthat::expect_false(".pq_min" %in% names(attr(x, "monoids", exact = TRUE)))
  testthat::expect_false(".pq_max" %in% names(attr(x, "monoids", exact = TRUE)))
  testthat::expect_equal(x[["kx"]]$item, "x")
  testthat::expect_equal(x[["kx"]]$priority, 2)

  x_unnamed <- as_flexseq(priority_queue("x", "y", priorities = c(2, 1)))
  x2 <- push_back(x_unnamed, list(item = "z", priority = 3))
  testthat::expect_s3_class(x2, "flexseq")
  testthat::expect_equal(length(x2), 3L)
})

testthat::test_that("lapply maps priority queue items and priorities", {
  q <- priority_queue("a", "bb", "ccc", priorities = c(1, 3, 2))
  q2 <- lapply(
    q,
    function(item, priority, name) {
      list(item = toupper(item), priority = priority + 2 * nchar(item))
    }
  )

  testthat::expect_equal(peek_min(q2), "A")
  testthat::expect_equal(peek_max(q2), "CCC")
  testthat::expect_identical(length(q2), 3L)
})

testthat::test_that("lapply respects sequence order for ties", {
  q <- as_priority_queue(
    c("a", "b", "c"),
    priorities = c(2, 1, 3)
  )
  q2 <- lapply(
    q,
    function(item, priority, name) list(priority = 1)
  )

  # All priorities tie; left-most in sequence wins.
  testthat::expect_equal(peek_min(q2), "a")
})

testthat::test_that("lapply can update priority queue entry names", {
  q <- as_priority_queue(setNames(as.list(c("x", "y")), c("kx", "ky")), priorities = c(2, 1))
  q2 <- lapply(q, function(item, priority, name) {
    list(name = paste0(name, "_new"))
  })

  testthat::expect_equal(q2[["kx_new"]]$item, "x")
  testthat::expect_equal(q2[["ky_new"]]$item, "y")
})

testthat::test_that("lapply validates priority queue inputs", {
  q <- priority_queue("a", priorities = 1)
  testthat::expect_error(lapply.priority_queue(as_flexseq(1:3), FUN = identity), "`q` must be a priority_queue")
  testthat::expect_error(lapply(q, 1), "`FUN` must be a function")
  testthat::expect_error(lapply(q, function(item, priority, name) 1), "`f` must return a list")
  testthat::expect_error(
    lapply(q, function(item, priority, name) list(123)),
    "must return a named list"
  )
  testthat::expect_error(
    lapply(q, function(item, priority, name) list(foo = 1)),
    "unsupported field"
  )
  testthat::expect_error(
    lapply(q, function(item, priority, name) list(priority = NA_real_)),
    "`priority` must be a single non-missing numeric value"
  )
})

testthat::test_that("priority_queue blocks split/locate helpers", {
  q <- priority_queue("a", "b", priorities = c(2, 1))
  testthat::expect_error(split_by_predicate(q, function(v) v >= 1, ".size"), "Cast first")
  testthat::expect_error(split_around_by_predicate(q, function(v) v >= 1, ".size"), "Cast first")
  testthat::expect_error(split_at(q, 1), "Cast first")
  testthat::expect_error(locate_by_predicate(q, function(v) v >= 1, ".size"), "Cast first")
})
