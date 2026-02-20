testthat::test_that("interval_index constructor sorts by start and preserves stable duplicate order", {
  ix <- as_interval_index(
    list("c", "a", "b", "d"),
    start = c(2, 1, 2, 2),
    end = c(5, 3, 4, 6)
  )

  testthat::expect_s3_class(ix, "interval_index")
  testthat::expect_equal(as.list(ix), list("a", "c", "b", "d"))
  testthat::expect_identical(length(ix), 4L)

  b <- interval_bounds(ix)
  testthat::expect_equal(lapply(b$start, identity), as.list(c(1, 2, 2, 2)))
  testthat::expect_equal(lapply(b$end, identity), as.list(c(3, 5, 4, 6)))
})

testthat::test_that("interval_index validates endpoints and bounds", {
  ok <- as_interval_index("p", start = 2, end = 2)
  testthat::expect_s3_class(ok, "interval_index")

  testthat::expect_error(as_interval_index("x", start = 3, end = 2), "start")
  testthat::expect_error(as_interval_index("x", start = 1, end = 2, bounds = "bad"), "bounds")
})

testthat::test_that("insert is persistent and appends at right edge of equal-start block", {
  ix <- as_interval_index(list("a", "b", "c"), start = c(1, 2, 2), end = c(2, 3, 4))
  ix2 <- insert(ix, "d", start = 2, end = 5)

  testthat::expect_equal(as.list(ix), list("a", "b", "c"))
  testthat::expect_equal(as.list(ix2), list("a", "b", "c", "d"))

  b <- interval_bounds(ix2)
  testthat::expect_equal(lapply(b$start, identity), as.list(c(1, 2, 2, 2)))
  testthat::expect_equal(lapply(b$end, identity), as.list(c(2, 3, 4, 5)))
})

testthat::test_that("find_point honors boundary modes", {
  ix <- as_interval_index(
    list("A", "B", "C", "D"),
    start = c(1, 2, 3, 2),
    end = c(2, 3, 4, 2),
    bounds = "[)"
  )

  testthat::expect_equal(as.list(find_point(ix, 2, bounds = "[)")), list("B"))
  testthat::expect_equal(as.list(find_point(ix, 2, bounds = "[]")), list("A", "B", "D"))
  testthat::expect_equal(as.list(find_point(ix, 2, bounds = "()")), list())
  testthat::expect_equal(as.list(find_point(ix, 2, bounds = "(]")), list("A"))
})

testthat::test_that("overlap/contain/within queries are deterministic", {
  ix <- as_interval_index(
    list("A", "B", "C", "D"),
    start = c(1, 2, 3, 2),
    end = c(2, 3, 4, 2),
    bounds = "[)"
  )

  testthat::expect_equal(as.list(find_overlaps(ix, 2, 3, bounds = "[)")), list("B"))
  testthat::expect_equal(as.list(find_overlaps(ix, 2, 3, bounds = "[]")), list("A", "B", "D", "C"))

  jy <- as_interval_index(
    list("outer", "inner", "tail", "point"),
    start = c(1, 2, 2, 3),
    end = c(5, 3, 5, 3),
    bounds = "[]"
  )

  testthat::expect_equal(as.list(find_containing(jy, 2, 3)), list("outer", "inner", "tail"))
  testthat::expect_equal(as.list(find_within(jy, 2, 3)), list("inner", "point"))
})

testthat::test_that("pop helpers follow first/all contracts and preserve persistence", {
  ix <- as_interval_index(
    list("A", "B", "C", "D"),
    start = c(1, 2, 3, 2),
    end = c(2, 3, 4, 2),
    bounds = "[]"
  )

  first <- pop_overlaps(ix, 2, 3)
  testthat::expect_equal(first$element, "A")
  testthat::expect_equal(first$start, 1)
  testthat::expect_equal(first$end, 2)
  testthat::expect_s3_class(first$remaining, "interval_index")

  all <- pop_overlaps(ix, 2, 3, which = "all")
  testthat::expect_s3_class(all$element, "interval_index")
  testthat::expect_null(all$start)
  testthat::expect_null(all$end)
  testthat::expect_equal(as.list(all$element), list("A", "B", "D", "C"))
  testthat::expect_identical(length(all$remaining), 0L)

  miss_first <- pop_within(ix, 9, 10)
  testthat::expect_null(miss_first$element)
  testthat::expect_null(miss_first$start)
  testthat::expect_null(miss_first$end)
  testthat::expect_equal(as.list(miss_first$remaining), as.list(ix))

  miss_all <- pop_containing(ix, 9, 10, which = "all")
  testthat::expect_s3_class(miss_all$element, "interval_index")
  testthat::expect_identical(length(miss_all$element), 0L)
  testthat::expect_equal(as.list(miss_all$remaining), as.list(ix))
})

testthat::test_that("ordered key APIs are blocked on interval_index", {
  ix <- as_interval_index(list("a", "b"), start = c(1, 2), end = c(2, 3))

  testthat::expect_error(lower_bound(ix, 2), "not supported for interval_index")
  testthat::expect_error(upper_bound(ix, 2), "not supported for interval_index")
  testthat::expect_error(peek_key(ix, 2), "not supported for interval_index")
  testthat::expect_error(pop_key(ix, 2), "not supported for interval_index")
  testthat::expect_error(elements_between(ix, 1, 2), "not supported for interval_index")
  testthat::expect_error(count_key(ix, 2), "not supported for interval_index")
  testthat::expect_error(count_between(ix, 1, 2), "not supported for interval_index")
})

testthat::test_that("interval_index indexing preserves class and blocks replacement", {
  ix <- as_interval_index(
    setNames(as.list(c("xa", "xb", "xc")), c("a", "b", "c")),
    start = c(1, 2, 3),
    end = c(2, 3, 4)
  )

  sub <- ix[c(1, 3)]
  testthat::expect_s3_class(sub, "interval_index")
  testthat::expect_equal(as.list(sub), list("xa", "xc"))

  testthat::expect_equal(ix[[2]], "xb")
  testthat::expect_equal(ix[["c"]], "xc")
  testthat::expect_equal(ix$b, "xb")

  testthat::expect_error(ix[c(3, 1)], "strictly increasing")
  testthat::expect_error(ix[c("b", "a")], "strictly increasing")

  testthat::expect_error({ ix[[1]] <- "qq" }, "not supported")
  testthat::expect_error({ ix[1] <- list("qq") }, "not supported")
  testthat::expect_error({ ix$b <- "qq" }, "not supported")
})

testthat::test_that("pop_front/pop_back preserve interval_index class", {
  ix <- as_interval_index("a", start = 1, end = 2)

  pf <- pop_front(ix)
  pb <- pop_back(ix)

  testthat::expect_equal(pf$element, "a")
  testthat::expect_s3_class(pf$remaining, "interval_index")
  testthat::expect_identical(length(pf$remaining), 0L)

  testthat::expect_equal(pb$element, "a")
  testthat::expect_s3_class(pb$remaining, "interval_index")
  testthat::expect_identical(length(pb$remaining), 0L)
})

testthat::test_that("fapply for interval_index updates payload only and keeps interval metadata immutable", {
  ix <- as_interval_index(
    setNames(as.list(c("a", "b", "c")), c("ka", "kb", "kc")),
    start = c(3, 1, 2),
    end = c(4, 2, 3),
    bounds = "[]"
  )
  b0 <- interval_bounds(ix)

  ix2 <- fapply(ix, function(item, start, end, name) {
    toupper(item)
  })

  testthat::expect_s3_class(ix2, "interval_index")
  testthat::expect_equal(unname(as.list(ix2)), list("B", "C", "A"))
  testthat::expect_equal(ix2[["kb"]], "B")

  b2 <- interval_bounds(ix2)
  testthat::expect_equal(unname(lapply(b2$start, identity)), unname(lapply(b0$start, identity)))
  testthat::expect_equal(unname(lapply(b2$end, identity)), unname(lapply(b0$end, identity)))

  testthat::expect_error(fapply(ix, 1), "`FUN` must be a function")

  ix3 <- fapply(ix, function(item, start, end, name) {
    list(old = item, at = c(start, end), nm = name)
  })
  testthat::expect_type(ix3[[1]], "list")
  testthat::expect_named(ix3[[1]], c("old", "at", "nm"))
  b3 <- interval_bounds(ix3)
  testthat::expect_equal(unname(lapply(b3$start, identity)), unname(lapply(b0$start, identity)))
  testthat::expect_equal(unname(lapply(b3$end, identity)), unname(lapply(b0$end, identity)))
})

testthat::test_that("interval_index recomputes user monoids across insert, fapply, and slices", {
  sum_item <- measure_monoid(function(a, b) a + b, 0, function(el) as.numeric(el$item))
  width_sum <- measure_monoid(function(a, b) a + b, 0, function(el) as.numeric(el$end - el$start))

  ix <- as_interval_index(
    as.list(c(10, 20, 30)),
    start = c(1, 2, 4),
    end = c(3, 5, 6),
    monoids = list(sum_item = sum_item, width_sum = width_sum)
  )
  testthat::expect_equal(node_measure(ix, "sum_item"), 60)
  testthat::expect_equal(node_measure(ix, "width_sum"), 7)

  ix2 <- insert(ix, 40, start = 3, end = 4)
  testthat::expect_equal(node_measure(ix2, "sum_item"), 100)
  testthat::expect_equal(node_measure(ix2, "width_sum"), 8)

  ix3 <- fapply(ix2, function(item, start, end, name) item + 1)
  testthat::expect_equal(node_measure(ix3, "sum_item"), 104)
  testthat::expect_equal(node_measure(ix3, "width_sum"), 8)

  overlaps <- find_overlaps(ix3, 2, 3, bounds = "[)")
  testthat::expect_s3_class(overlaps, "interval_index")
  testthat::expect_equal(as.list(overlaps), as.list(c(11, 21)))
  testthat::expect_equal(node_measure(overlaps, "sum_item"), 32)
  testthat::expect_equal(node_measure(overlaps, "width_sum"), 5)

  popped <- pop_overlaps(ix3, 2, 3, which = "all", bounds = "[)")
  testthat::expect_s3_class(popped$element, "interval_index")
  testthat::expect_s3_class(popped$remaining, "interval_index")
  testthat::expect_equal(node_measure(popped$element, "sum_item"), 32)
  testthat::expect_equal(node_measure(popped$remaining, "sum_item"), 72)
  testthat::expect_equal(node_measure(popped$element, "width_sum"), 5)
  testthat::expect_equal(node_measure(popped$remaining, "width_sum"), 3)
})

testthat::test_that("interval_index casts down to flexseq explicitly", {
  width_sum <- measure_monoid(`+`, 0, function(el) as.numeric(el$end - el$start))
  ix <- as_interval_index(
    setNames(list("x", "y"), c("ix", "iy")),
    start = c(1, 3),
    end = c(2, 5),
    monoids = list(width_sum = width_sum)
  )

  fx <- as_flexseq(ix)
  testthat::expect_s3_class(fx, "flexseq")
  testthat::expect_false(inherits(fx, "interval_index"))
  testthat::expect_equal(fx[["ix"]]$item, "x")
  testthat::expect_equal(fx[["ix"]]$start, 1)
  testthat::expect_equal(fx[["ix"]]$end, 2)

  ms <- names(attr(fx, "monoids", exact = TRUE))
  testthat::expect_true(all(c(".size", ".named_count", "width_sum") %in% ms))
  testthat::expect_false(".ivx_max_start" %in% ms)
  testthat::expect_false(".oms_max_key" %in% ms)
  testthat::expect_identical(node_measure(fx, "width_sum"), 3)
})
