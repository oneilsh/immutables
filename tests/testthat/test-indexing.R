testthat::test_that("read indexing supports [] and [[ with positive integer indices", {
  r <- MeasureMonoid(function(a, b) a + b, 0, function(el) el)
  t <- tree_from(1:6, monoids = list(sum = r))

  testthat::expect_equal(t[[3]], 3)

  s <- t[c(2, 4, 6)]
  testthat::expect_identical(reduce_left(s, r), 12)
  testthat::expect_identical(attr(s, "measures")$.size, 3)
})

testthat::test_that("read indexing supports duplicates and arbitrary order", {
  t <- tree_from(letters[1:5])
  cat_m <- MeasureMonoid(paste0, "", as.character)

  s <- t[c(5, 2, 2, 4)]
  testthat::expect_identical(reduce_left(s, cat_m), "ebbd")
  testthat::expect_identical(attr(s, "measures")$.size, 4)
})

testthat::test_that("indexing enforces bounds and integer-only indices", {
  r <- MeasureMonoid(function(a, b) a + b, 0, function(el) el)
  t <- tree_from(1:4, monoids = list(sum = r))

  testthat::expect_error(t[[0]], "positive integer")
  testthat::expect_error(t[[5]], "out of bounds")
  testthat::expect_error(t[[c(1, 2)]], "\\[\\[ expects exactly one index")
  testthat::expect_error(t[c(1, -2)], "positive integer")
  testthat::expect_error(t[c(1, 2.5)], "integer indices")
})

testthat::test_that("replacement indexing supports [[<- and [<- with exact-size semantics", {
  r <- MeasureMonoid(function(a, b) a + b, 0, function(el) el)
  t <- tree_from(1:5, monoids = list(sum = r))

  t2 <- t
  t2[[2]] <- 20
  testthat::expect_identical(reduce_left(t2, r), 33)
  testthat::expect_identical(t2[[2]], 20)

  t3 <- t
  t3[c(1, 5)] <- list(10, 50)
  testthat::expect_identical(reduce_left(t3, r), 69)
  testthat::expect_identical(t3[[1]], 10)
  testthat::expect_identical(t3[[5]], 50)

  testthat::expect_error({
    t4 <- t
    t4[c(1, 2)] <- list(99)
  }, "Replacement length must match index length exactly")
})

testthat::test_that("[[<- keeps monoid attrs and updates one element via split path", {
  sum_m <- MeasureMonoid(`+`, 0, as.numeric)
  t <- tree_from(1:6, monoids = list(sum = sum_m))
  t2 <- t
  t2[[4]] <- 99

  testthat::expect_identical(attr(t2, "measures")$.size, 6)
  testthat::expect_identical(attr(t2, "measures")$sum, 116)
  testthat::expect_identical(t2[[4]], 99)
})

testthat::test_that("[<- applies duplicate indices sequentially (last write wins)", {
  t <- tree_from(letters[1:5])
  t[c(2, 2)] <- list("X", "Y")
  testthat::expect_identical(t[[2]], "Y")
})

testthat::test_that("[<- supports arbitrary replacement order", {
  t <- tree_from(letters[1:5])
  t[c(5, 1, 4)] <- list("u", "v", "w")
  testthat::expect_identical(t[[1]], "v")
  testthat::expect_identical(t[[4]], "w")
  testthat::expect_identical(t[[5]], "u")
})

testthat::test_that("[<- keeps structural attrs and custom monoid measures coherent", {
  sum_m <- MeasureMonoid(`+`, 0, as.numeric)
  t <- tree_from(1:5, monoids = list(sum = sum_m))
  t[c(2, 4)] <- list(20, 40)

  testthat::expect_identical(attr(t, "measures")$.size, 5)
  testthat::expect_identical(attr(t, "measures")$sum, 69)
  testthat::expect_identical(reduce_left(t, sum_m), 69)
})

testthat::test_that("[<- zero-length index is a no-op when replacement is empty", {
  t <- tree_from(letters[1:5])
  cat_m <- MeasureMonoid(paste0, "", as.character)
  before <- reduce_left(t, cat_m)
  t2 <- t
  t2[integer(0)] <- list()

  testthat::expect_identical(reduce_left(t2, cat_m), before)
  testthat::expect_identical(attr(t2, "measures")$.size, attr(t, "measures")$.size)
})

testthat::test_that("size measure is available by default for indexing even if not provided", {
  sum_only <- MeasureMonoid(function(a, b) a + b, 0, function(el) el)
  t <- tree_from(1:3, monoids = list(sum = sum_only))
  ms <- attr(t, "measures")
  testthat::expect_identical(ms$.size, 3)
  testthat::expect_equal(t[[2]], 2)
})

testthat::test_that("single-element read indexing follows locate(.size) semantics", {
  t <- tree_from(letters[1:8])
  for(i in 1:8) {
    hit <- locate(t, function(v) v >= i, ".size")
    testthat::expect_true(hit$found)
    testthat::expect_identical(t[[i]], hit$elem)
  }
})
