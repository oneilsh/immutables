testthat::test_that("priority_queue constructor and length/is_empty", {
  q0 <- priority_queue()
  testthat::expect_true(inherits(q0, "priority_queue"))
  testthat::expect_true(inherits(q0, "flexseq"))
  testthat::expect_true(is_empty(q0))
  testthat::expect_identical(length(q0), 0L)

  q <- priority_queue("a", "b", "c", priorities = c(3, 1, 2))
  testthat::expect_false(is_empty(q))
  testthat::expect_identical(length(q), 3L)
  testthat::expect_equal(peek_min(q), "b")
  testthat::expect_equal(peek_max(q), "a")
})

testthat::test_that("min/max extraction is stable on ties", {
  q <- priority_queue("a", "b", "c", "d", priorities = c(2, 1, 1, 2))

  e1 <- extract_min(q)
  testthat::expect_equal(e1$element, "b")
  testthat::expect_equal(e1$priority, 1)

  e2 <- extract_min(e1$queue)
  testthat::expect_equal(e2$element, "c")
  testthat::expect_equal(e2$priority, 1)

  x1 <- extract_max(q)
  testthat::expect_equal(x1$element, "a")
  testthat::expect_equal(x1$priority, 2)

  x2 <- extract_max(x1$queue)
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

testthat::test_that("priority queue carries required monoids", {
  q <- priority_queue(1, 2, priorities = c(10, 20))
  ms <- names(attr(q, "monoids", exact = TRUE))
  testthat::expect_true(all(c(".size", ".named_count", ".pq_min", ".pq_max") %in% ms))
})

testthat::test_that("values path is removed", {
  testthat::expect_error(as_flexseq(1:3, values = 1:3), "unused argument")
  testthat::expect_error(tree_from(1:3, values = 1:3), "unused argument")
})

testthat::test_that("apply maps priority queue items and priorities", {
  q <- priority_queue("a", "bb", "ccc", priorities = c(1, 3, 2))
  q2 <- apply(
    q,
    function(item, priority, seq_id, name) {
      list(item = toupper(item), priority = priority + 2 * nchar(item))
    }
  )

  testthat::expect_equal(peek_min(q2), "A")
  testthat::expect_equal(peek_max(q2), "CCC")
  testthat::expect_identical(length(q2), 3L)
})

testthat::test_that("apply can preserve priority queue seq_id tie ordering", {
  q <- as_priority_queue(
    c("a", "b", "c"),
    priorities = c(2, 1, 3)
  )
  q2 <- apply(
    q,
    function(item, priority, seq_id, name) list(priority = 1),
    reset_ties = FALSE
  )

  # All priorities tie; earliest original seq_id wins.
  testthat::expect_equal(peek_min(q2), "a")
})

testthat::test_that("apply can update priority queue entry names", {
  q <- as_priority_queue(setNames(as.list(c("x", "y")), c("kx", "ky")), priorities = c(2, 1))
  q2 <- apply(q, function(item, priority, seq_id, name) {
    list(name = paste0(name, "_new"))
  })

  testthat::expect_equal(q2[["kx_new"]]$item, "x")
  testthat::expect_equal(q2[["ky_new"]]$item, "y")
})

testthat::test_that("apply validates priority queue inputs", {
  q <- priority_queue("a", priorities = 1)
  testthat::expect_error(apply.priority_queue(as_flexseq(1:3), FUN = identity), "`q` must be a priority_queue")
  testthat::expect_error(apply(q, 1), "`FUN` must be a function")
  testthat::expect_error(
    apply(q, function(item, priority, seq_id, name) list(), reset_ties = NA),
    "`reset_ties` must be TRUE or FALSE"
  )
  testthat::expect_error(apply(q, function(item, priority, seq_id, name) 1), "`f` must return a list")
  testthat::expect_error(
    apply(q, function(item, priority, seq_id, name) list(123)),
    "must return a named list"
  )
  testthat::expect_error(
    apply(q, function(item, priority, seq_id, name) list(foo = 1)),
    "unsupported field"
  )
  testthat::expect_error(
    apply(q, function(item, priority, seq_id, name) list(priority = NA_real_)),
    "`priority` must be a single non-missing numeric value"
  )
})
