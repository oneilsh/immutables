testthat::test_that("split_around_by_predicate returns distinguished element and context", {
  mr <- measure_monoid(function(a, b) a + b, 0, function(el) 1)
  t <- as_flexseq(letters[1:6], monoids = list(count = mr))

  s <- split_around_by_predicate(t, function(v) v >= 4, ".size")
  testthat::expect_identical(s$elem, "d")
  testthat::expect_identical(attr(s$left, "measures")$.size, 3)
  testthat::expect_identical(attr(s$right, "measures")$.size, 2)
})

testthat::test_that("split_by_predicate returns left/right trees with split element prepended to right", {
  mr <- measure_monoid(function(a, b) a + b, 0, function(el) 1)
  sr <- measure_monoid(function(a, b) paste0(a, b), "", function(el) "")
  t <- as_flexseq(letters[1:6], monoids = list(count = mr))

  s <- split_by_predicate(t, function(v) v >= 4, ".size")
  testthat::expect_identical(attr(s$left, "measures")$.size, 3)
  testthat::expect_identical(attr(s$right, "measures")$.size, 3)
  testthat::expect_identical(fold_left(s$left, sr), "abc")
  testthat::expect_identical(fold_left(s$right, sr), "def")
})

testthat::test_that("split_by_predicate handles empty and predicate-never-true cases", {
  e <- flexseq()
  s0 <- split_by_predicate(e, function(v) v >= 1, ".size")
  testthat::expect_true(s0$left %isa% Empty)
  testthat::expect_true(s0$right %isa% Empty)

  t <- as_flexseq(letters[1:4])
  s <- split_by_predicate(t, function(v) v >= 10, ".size")
  testthat::expect_identical(attr(s$left, "measures")$.size, 4)
  testthat::expect_true(s$right %isa% Empty)
})

testthat::test_that("split_by_predicate boundary at first and last element", {
  sr <- measure_monoid(function(a, b) paste0(a, b), "", function(el) "")
  t <- as_flexseq(letters[1:5])

  s1 <- split_by_predicate(t, function(v) v >= 1, ".size")
  testthat::expect_true(s1$left %isa% Empty)
  testthat::expect_identical(attr(s1$right, "measures")$.size, 5)
  testthat::expect_identical(fold_left(s1$right, sr), "abcde")

  s2 <- split_by_predicate(t, function(v) v >= 5, ".size")
  testthat::expect_identical(attr(s2$left, "measures")$.size, 4)
  testthat::expect_identical(attr(s2$right, "measures")$.size, 1)
  testthat::expect_identical(fold_left(s2$left, sr), "abcd")
  testthat::expect_identical(fold_left(s2$right, sr), "e")
})

testthat::test_that("split_around_by_predicate respects custom accumulator offsets", {
  t <- as_flexseq(letters[1:5])

  s <- split_around_by_predicate(t, function(v) v >= 8, ".size", accumulator = 5)
  testthat::expect_identical(s$elem, "c")
  testthat::expect_identical(attr(s$left, "measures")$.size, 2)
  testthat::expect_identical(attr(s$right, "measures")$.size, 2)
})

testthat::test_that("split_around_by_predicate errors when monoid name is missing", {
  t <- as_flexseq(letters[1:4])
  testthat::expect_error(split_around_by_predicate(t, function(v) v >= 2, "unknown"), "not found")
})

testthat::test_that("split_at supports scalar integer index", {
  t <- as_flexseq(letters[1:6])

  s <- split_at(t, 4)
  testthat::expect_identical(fold_left(s$left, measure_monoid(function(a, b) paste0(a, b), "", function(el) as.character(el))), "abc")
  testthat::expect_identical(s$elem, "d")
  testthat::expect_identical(fold_left(s$right, measure_monoid(function(a, b) paste0(a, b), "", function(el) as.character(el))), "ef")

  s2 <- split_at(t, 4, pull_index = TRUE)
  testthat::expect_identical(fold_left(s2$left, measure_monoid(function(a, b) paste0(a, b), "", function(el) as.character(el))), "abc")
  testthat::expect_identical(fold_left(s2$right, measure_monoid(function(a, b) paste0(a, b), "", function(el) as.character(el))), "def")
})

testthat::test_that("split_at supports scalar name", {
  t <- as_flexseq(setNames(as.list(letters[1:6]), LETTERS[1:6]))

  s <- split_at(t, "D")
  testthat::expect_identical(.ft_strip_name(s$elem), "d")
  testthat::expect_identical(attr(s$left, "measures")$.size, 3)
  testthat::expect_identical(attr(s$right, "measures")$.size, 2)

  s2 <- split_at(t, "D", pull_index = TRUE)
  testthat::expect_identical(attr(s2$left, "measures")$.size, 3)
  testthat::expect_identical(attr(s2$right, "measures")$.size, 3)
})
