# CRAN Cleanup Plan — Detailed Implementation Guide

This document provides step-by-step instructions for each change. Follow the
execution order exactly. After all code changes, run `roxygen2::roxygenise()`
to regenerate NAMESPACE and man/ pages, then run `R CMD check`.

All file paths are relative to the package root:
`/Users/oneilsh/Documents/projects/r_stuff/functional_stuff/immutables`

---

## Phase 1: Trivial fixes (Items 3, 7, 9, 13)

### 1.1 Remove `LazyData: true` from DESCRIPTION

**File:** `DESCRIPTION`

Remove the line `LazyData: true` (currently line 15). The package has no `data/`
directory; this line produces an R CMD check NOTE.

### 1.2 Add `meta/` to `.Rbuildignore`

**File:** `.Rbuildignore`

Append this line at the end of the file:

```
^meta$
```

(The `^meta$` pattern is sufficient because R's build ignore treats it as a
directory match.)

### 1.3 Add `BugReports` to DESCRIPTION

**File:** `DESCRIPTION`

Add the following line after the existing `URL:` line (currently line 4):

```
BugReports: https://github.com/oneilsh/immutables/issues
```

### 1.4 Delete placeholder file

**Delete:** `R/fingertree_measured.R`

This file contains only a TODO comment. Remove it entirely.

---

## Phase 2: Remove PascalCase aliases (Item 12)

### 2.1 Remove `MeasureMonoid` alias function

**File:** `R/MeasureMonoid.R`

The file currently defines two functions. Remove the `MeasureMonoid` alias
(lines 24-27). The file should end after `measure_monoid()` definition (line 22).
Keep the `measure_monoid` function and its roxygen block entirely intact.

**Before (lines 23-27):**
```r
# Runtime: O(1).
MeasureMonoid <- function(f, i, measure) {
  measure_monoid(f, i, measure)
}
```

**After:** Delete those lines entirely.

### 2.2 Update internal callers of `MeasureMonoid()`

The internal code calls `MeasureMonoid()` in exactly two places. These are inside
`lambda.r`-typed functions so the class name `"MeasureMonoid"` in `%::%` type
annotations must NOT be changed — only the function *calls*.

**File: `R/measured.R`, line 17:**
Change: `MeasureMonoid(function(a, b) a + b, 0, function(el) 1)`
To: `measure_monoid(function(a, b) a + b, 0, function(el) 1)`

**File: `R/measured.R`, line 24:**
Change: `MeasureMonoid(function(a, b) a + b, 0L, function(el) {`
To: `measure_monoid(function(a, b) a + b, 0L, function(el) {`

**IMPORTANT:** Do NOT change:
- The `"MeasureMonoid"` string in the `class()` assignment on `R/MeasureMonoid.R:20`.
  This is the class name used for `inherits()` checks, not a function call.
- The `%::% ... : MeasureMonoid : .` type annotations in `R/fold_left.R`,
  `R/fold_right.R`, `R/measured.R`, `R/locate_impl.R`. These are lambda.r type
  annotations referencing the *class*, not the function.
- The `inherits(r, "MeasureMonoid")` check in `R/measured.R:4`.

### 2.3 Remove `Predicate` alias function

**File:** `R/Predicate.R`

Remove the `Predicate` alias (lines 22-25). The file should end after the
`predicate()` function definition (line 20).

**Before (lines 22-25):**
```r
# Runtime: O(1).
Predicate <- function(f) {
  predicate(f)
}
```

**After:** Delete those lines entirely.

### 2.4 Update test referencing `Predicate`

**File:** `tests/testthat/test-cpp-parity.R`

Line 79: The test name says `"backend parity: Predicate constructor/use"`.
Change this to `"backend parity: predicate constructor/use"`.

The test body already uses `predicate()` (lowercase) on line 81 — no code change
needed inside the test, just the description string.

---

## Phase 3: Eliminate `rlist` dependency (Item 6)

### 3.1 Replace `list.prepend` and `list.append` calls

There are exactly 3 call sites. Replace each with base R:

**File: `R/add_left.R`, line 26:**

Change: `newd <- list.prepend(d, el)`
To: `newd <- c(list(el), d)`

(The surrounding code on lines 25 and 27 already saves/restores class attributes,
so this replacement is safe.)

**File: `R/add_right.R`, line 23:**

Change: `newd <- list.append(d, el)`
To: `newd <- c(d, list(el))`

(Same pattern — class is restored on line 24.)

**File: `R/utils.R`, line 189:**

Change: `rest = list.prepend(rest, first)`
To: `rest = c(list(first), rest)`

### 3.2 Remove `rlist` from package imports

**File: `R/fingertree-package.R`, line 7:**

Delete the line: `#' @import rlist`

**File: `DESCRIPTION`:**

Remove `rlist,` from the `Imports:` section (currently line 19). Ensure the
remaining imports have correct comma formatting:

```
Imports:
    Rcpp,
    lambda.r,
    igraph
```

(Note: `igraph` moves to Suggests in Phase 4, but do Phase 3 first.)

---

## Phase 4: Move `igraph` to Suggests (Item 5)

### 4.1 Move igraph in DESCRIPTION

**File: `DESCRIPTION`**

Remove `igraph` from `Imports:` (after Phase 3 it will be the last entry).
Add it to `Suggests:` (which currently lists pkgdown and testthat).

Result:

```
Imports:
    Rcpp,
    lambda.r
Suggests:
    igraph,
    pkgdown,
    testthat (>= 3.0.0)
```

### 4.2 Remove `@import igraph` from package declaration

**File: `R/fingertree-package.R`, line 8:**

Delete the line: `#' @import igraph`

After Phase 3 and 4.2, the file should look like:

```r
#' immutables
#'
#' Monoid-annotated 2-3 finger trees.
#'
#' @keywords internal
#' @import lambda.r
#' @importFrom Rcpp evalCpp
#' @useDynLib immutables, .registration = TRUE
"_PACKAGE"
```

### 4.3 Add requireNamespace guard to `plot_tree()`

**File: `R/utils.R`**

In the `plot_tree()` function (starts at line 125), add a guard at the very
beginning of the function body, before any other code (after line 127):

```r
  if(!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for plot_tree(). Install it with install.packages('igraph').")
  }
```

### 4.4 Namespace-qualify igraph function calls in `plot_tree()`

In the same function, qualify all igraph calls:

**Line 139:** Change `graph_from_data_frame(...)` to `igraph::graph_from_data_frame(...)`
**Line 145:** Change `vcount(g)` to `igraph::vcount(g)`
**Line 147:** Change `V(g)$type` to `igraph::V(g)$type`
**Line 149:** Change `V(g)$label` to `igraph::V(g)$label` (both occurrences on this line); also `V(g)$type`
**Line 151:** Change `V(g)$label` to `igraph::V(g)$label`
**Line 155:** Change `layout_as_tree(g)` to `igraph::layout_as_tree(g)`

---

## Phase 5: Convert `split` and `append` to S3 methods (Item 4)

### 5.1 Convert `split` to `split.flexseq`

**File: `R/split.R`**

Replace the entire file contents. The key changes:
- Roxygen: add `@method split flexseq` tag
- Remove the `@export` on the standalone function (it will be `S3method` instead)
- Rename parameters: `t` → `x`, `predicate` → `f`, third arg via `...`
- `monoid_name` is extracted from `...` (first positional element in `...`)

Replace the file with:

```r
#' Split a flexseq into left and right parts
#'
#' Splits a sequence at the point where the predicate first becomes TRUE on
#' accumulated monoid measures.
#'
#' @method split flexseq
#' @param x A `flexseq`.
#' @param f Predicate function on accumulated measure values.
#' @param ... Additional arguments. The first positional argument is
#'   `monoid_name` (character scalar naming the monoid to scan).
#' @return A list with `left` and `right` flexseq objects.
#' @examples
#' t <- as_flexseq(letters[1:6])
#' s <- split(t, function(v) v >= 4, ".size")
#' length(s$left)
#' length(s$right)
#' @export
# Runtime: O(log n) near split point depth.
split.flexseq <- function(x, f, ...) {
  dots <- list(...)
  if(length(dots) < 1L || !is.character(dots[[1L]])) {
    stop("`monoid_name` (character) is required as the third argument to split().")
  }
  monoid_name <- dots[[1L]]

  ctx <- resolve_named_monoid(x, monoid_name)
  ms <- ctx$monoids
  mr <- ctx$monoid

  if(x %isa% Empty) {
    return(list(left = .as_flexseq(measured_empty(ms)), right = .as_flexseq(measured_empty(ms))))
  }

  if(f(node_measure(x, monoid_name))) {
    s <- if(.ft_cpp_can_use(ms)) {
      .ft_cpp_split_tree(x, f, ms, monoid_name, mr$i)
    } else {
      split_tree_impl_fast(f, mr$i, x, ms, mr, monoid_name)
    }
    right <- prepend(s$right, s$elem)
    return(list(left = .as_flexseq(s$left), right = .as_flexseq(right)))
  }

  list(left = .as_flexseq(x), right = .as_flexseq(measured_empty(ms)))
}
```

**NAMESPACE effect:** After `roxygenise()`, the NAMESPACE will contain
`S3method(split,flexseq)` instead of `export(split)`. The existing
`S3method(split,flexseq)` that was not present before will be added.

### 5.2 Convert `append` to S3 generic + `append.flexseq`

**File: `R/append.R`**

Replace the entire file contents. The key changes:
- Define `append` as a new S3 generic with `UseMethod`
- Provide `append.default` that delegates to `base::append`
- Provide `append.flexseq` that wraps the current implementation
- Parameter names: `x` (tree), `values` (element to append)

Replace the file with:

```r
#' Append elements
#'
#' Generic function for appending. The default method calls [base::append()].
#' The flexseq method appends a single element to the right side of a
#' persistent sequence.
#'
#' @param x Object to append to.
#' @param ... Method-specific arguments.
#' @return Updated object.
#' @export
append <- function(x, ...) {
  UseMethod("append")
}

#' @rdname append
#' @param values Values to append (for default method, passed to [base::append()]).
#' @param after Position after which to insert (for default method).
#' @export
append.default <- function(x, values, after = length(x), ...) {
  base::append(x, values, after)
}

#' Append an element to a flexseq
#'
#' @method append flexseq
#' @param x A `flexseq`.
#' @param values Element to append.
#' @param ... Unused.
#' @return Updated `flexseq`.
#' @examples
#' t <- as_flexseq(letters[1:3])
#' t2 <- append(t, "d")
#' t2[[4]]
#'
#' # Append to a named sequence
#' tn <- as_flexseq(setNames(as.list(1:2), c("a", "b")))
#' tn2 <- append(tn, stats::setNames(3, "c"))
#' tn2[["c"]]
#' @export
# Runtime: O(log n) tree update, with O(1) local name-state checks.
append.flexseq <- function(x, values, ...) {
  ms <- attr(x, "monoids", exact = TRUE)
  if(is.null(ms)) {
    stop("Tree has no monoids attribute.")
  }
  m <- attr(x, "measures", exact = TRUE)
  if(is.null(m)) {
    stop("Tree has no measures attribute.")
  }
  n <- as.integer(m[[".size"]])
  nn <- as.integer(m[[".named_count"]])
  # Runtime: O(1). Enforce non-mixed naming invariant without full traversal.
  if(n > 0L && nn != 0L && nn != n) {
    stop("Invalid tree name state: mixed named/unnamed elements.")
  }

  x2 <- values
  if(nn == 0L) {
    # Fast unnamed path: avoid attr writes when incoming element is also unnamed.
    nm <- .ft_get_name(values)
    if(is.null(nm)) {
      nm <- .ft_name_from_value(values)
    }
    if(is.null(nm)) {
      if(.ft_cpp_can_use(ms)) {
        return(.as_flexseq(.ft_cpp_add_right(x, x2, ms)))
      }
      return(.as_flexseq(.add_right_fast(x, x2, ms)))
    }
    if(n > 0L) {
      stop("Cannot mix named and unnamed elements (append would create mixed named and unnamed tree).")
    }
    if(.ft_cpp_can_use(ms)) {
      return(.as_flexseq(.ft_cpp_add_right_named(x, values, nm, ms)))
    }
    x2 <- .ft_set_name(x2, nm)
  } else {
    nm <- .ft_effective_name(values)
    if(is.null(nm)) {
      stop("Cannot mix named and unnamed elements (append would create mixed named and unnamed tree).")
    }
    if(.ft_cpp_can_use(ms)) {
      return(.as_flexseq(.ft_cpp_add_right_named(x, values, nm, ms)))
    }
    x2 <- .ft_set_name(x2, nm)
  }

  if(.ft_cpp_can_use(ms)) {
    return(.as_flexseq(.ft_cpp_add_right(x, x2, ms)))
  }
  .as_flexseq(.add_right_fast(x, x2, ms))
}
```

**NAMESPACE effect:** After `roxygenise()`, the NAMESPACE will contain:
- `export(append)` (the generic)
- `S3method(append,default)`
- `S3method(append,flexseq)`

### 5.3 Update internal callers of `append()`

There is one internal call to `append()` inside `R/indexing.R`:

**File: `R/indexing.R`, line 725:**
```r
  left_plus <- append(s$left, value)
```

`s$left` is a flexseq (from `split_tree` which returns flexseq objects), so this
call will correctly dispatch to `append.flexseq`. **No change needed.**

There is also one internal call in `R/priority_queue.R`:

**File: `R/priority_queue.R`, line 164:**
```r
  q2 <- append(q, entry)
```

`q` is a `priority_queue` which inherits from `flexseq`, so `append.flexseq`
will be found via S3 dispatch. **No change needed.**

### 5.4 Verify test call sites

All existing test calls like `append(t, x)` and `split(t, pred, ".size")` will
continue to work because:
- `append` dispatches on the first arg's class (flexseq)
- `split` dispatches on the first arg's class (flexseq)
- Positional arguments are unchanged

**No test changes are needed for the `split` or `append` refactoring itself.**
The test description on line 79 of `test-cpp-parity.R` (already changed in
Phase 2.4) is the only test file edit so far.

---

## Phase 6: Fix all `@examples` blocks (Item 1)

Every `@examples` block must use ONLY exported, user-facing functions. Replace
all occurrences of non-exported functions with their public equivalents.

Rules:
- `MeasureMonoid(...)` → `measure_monoid(...)`
- `tree_from(...)` → `as_flexseq(...)`
- `Predicate(...)` → `predicate(...)`
- `concat_trees(x, y)` → `c(x, y)` (since both are flexseq and c.flexseq exists)
- `empty_tree(...)` → `flexseq()`  (with monoids if needed: `as_flexseq(list(), monoids = ...)`)
- `get_graph_df(t)` → wrap in `\dontrun{}`
- `plot_tree(t)` → wrap in `\dontrun{}`

Below is the exhaustive list of files and the exact example replacements needed.

### 6.1 `R/locate.R` (exported function — examples MUST run)

Replace the `@examples` block (lines 14-21):

**Before:**
```r
#' @examples
#' t <- tree_from(letters[1:6])
#' locate(t, function(v) v >= 4, ".size")
#'
#' # Metadata-rich locate for custom monoid
#' sum_m <- MeasureMonoid(`+`, 0, as.numeric)
#' t2 <- tree_from(1:6, monoids = list(sum = sum_m))
#' locate(t2, function(v) v >= 10, "sum", include_metadata = TRUE)
```

**After:**
```r
#' @examples
#' t <- as_flexseq(letters[1:6])
#' locate(t, function(v) v >= 4, ".size")
#'
#' # Metadata-rich locate for custom monoid
#' sum_m <- measure_monoid(`+`, 0, as.numeric)
#' t2 <- as_flexseq(1:6, monoids = list(sum = sum_m))
#' locate(t2, function(v) v >= 10, "sum", include_metadata = TRUE)
```

### 6.2 `R/split.R` (already rewritten in Phase 5.1)

The new file already has correct examples using `as_flexseq`. No further action.

### 6.3 `R/split_tree.R` (exported function — examples MUST run)

Replace the `@examples` block (lines 8-15):

**Before:**
```r
#' @examples
#' t <- tree_from(letters[1:6])
#' s <- split_tree(t, function(v) v >= 4, ".size")
#' s$elem
#'
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' fold_left(s$left, cat_m)
#' fold_left(s$right, cat_m)
```

**After:**
```r
#' @examples
#' t <- as_flexseq(letters[1:6])
#' s <- split_tree(t, function(v) v >= 4, ".size")
#' s$elem
#'
#' cat_m <- measure_monoid(paste0, "", as.character)
#' fold_left(s$left, cat_m)
#' fold_left(s$right, cat_m)
```

### 6.4 `R/add_monoids.R` (exported function — examples MUST run)

Replace the `@examples` block and the `@param monoids` description:

**Line 4, change `@param` description:**
Change: ``@param monoids Named list of `MeasureMonoid` objects to add.``
To: ``@param monoids Named list of `measure_monoid` objects to add.``

**Replace `@examples` block (lines 7-16):**

**Before:**
```r
#' @examples
#' t <- tree_from(1:5)
#' sum_m <- MeasureMonoid(`+`, 0, as.numeric)
#' t2 <- add_monoids(t, list(sum = sum_m))
#' attr(t2, "measures")$sum
#'
#' # Replace an existing monoid definition
#' sum2 <- MeasureMonoid(function(a, b) a + 2 * b, 0, as.numeric)
#' t3 <- add_monoids(t2, list(sum = sum2), overwrite = TRUE)
#' attr(t3, "measures")$sum
```

**After:**
```r
#' @examples
#' t <- as_flexseq(1:5)
#' sum_m <- measure_monoid(`+`, 0, as.numeric)
#' t2 <- add_monoids(t, list(sum = sum_m))
#' attr(t2, "measures")$sum
#'
#' # Replace an existing monoid definition
#' sum2 <- measure_monoid(function(a, b) a + 2 * b, 0, as.numeric)
#' t3 <- add_monoids(t2, list(sum = sum2), overwrite = TRUE)
#' attr(t3, "measures")$sum
```

### 6.5 `R/validate.R` (exported functions — examples MUST run)

**`validate_tree` example (line 12):**
Change: `#' t <- tree_from(letters[1:10])`
To: `#' t <- as_flexseq(letters[1:10])`

**`validate_name_state` example (line 32):**
Change: `#' t <- tree_from(setNames(as.list(letters[1:4]), letters[1:4]))`
To: `#' t <- as_flexseq(setNames(as.list(letters[1:4]), letters[1:4]))`

### 6.6 `R/dollar.R` (exported functions — examples MUST run)

**`$.FingerTree` example (line 21):**
Change: `#' t <- tree_from(setNames(as.list(1:3), c("a", "b", "c")))`
To: `#' t <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))`

**`$<-.FingerTree` example (line 50):**
Change: `#' t <- tree_from(setNames(as.list(1:3), c("a", "b", "c")))`
To: `#' t <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))`

### 6.7 `R/indexing.R` (exported functions — examples MUST run)

This file has many examples. Replace every `tree_from` and `MeasureMonoid`:

**`[.FingerTree` examples (around lines 410-425):**

Line 411: Change `#' t <- tree_from(letters[1:6])` to `#' t <- as_flexseq(letters[1:6])`
Line 413: Change `#' cat_m <- MeasureMonoid(paste0, "", as.character)` to `#' cat_m <- measure_monoid(paste0, "", as.character)`
Line 420: Change `#' tn <- tree_from(setNames(as.list(letters[1:4]), c("w", "x", "y", "z")))` to `#' tn <- as_flexseq(setNames(as.list(letters[1:4]), c("w", "x", "y", "z")))`
Line 422: Change `#' fold_left(out, MeasureMonoid(paste0, "", function(el) if(is.null(el)) "_" else el))` to `#' fold_left(out, measure_monoid(paste0, "", function(el) if(is.null(el)) "_" else el))`

**`[[.FingerTree` examples (around lines 486-490):**

Line 486: Change `#' t <- tree_from(letters[1:5])` to `#' t <- as_flexseq(letters[1:5])`
Line 489: Change `#' tn <- tree_from(setNames(as.list(letters[1:3]), c("a1", "a2", "a3")))` to `#' tn <- as_flexseq(setNames(as.list(letters[1:3]), c("a1", "a2", "a3")))`

**`[<-.FingerTree` examples (around lines 521-534):**

Line 521: Change `#' t <- tree_from(1:6)` to `#' t <- as_flexseq(1:6)`
Line 523: Change `#' sum_m <- MeasureMonoid(\`+\`, 0, as.numeric)` to `#' sum_m <- measure_monoid(\`+\`, 0, as.numeric)`
Line 530: Change `#' tn <- tree_from(setNames(as.list(1:4), c("a", "b", "c", "d")))` to `#' tn <- as_flexseq(setNames(as.list(1:4), c("a", "b", "c", "d")))`

**`[[<-.FingerTree` examples (around lines 662-674):**

Line 662: Change `#' t <- tree_from(letters[1:4])` to `#' t <- as_flexseq(letters[1:4])`
Line 664: Change `#' cat_m <- MeasureMonoid(paste0, "", as.character)` to `#' cat_m <- measure_monoid(paste0, "", as.character)`
Line 667: Change `#' tn <- tree_from(setNames(as.list(1:3), c("x", "y", "z")))` to `#' tn <- as_flexseq(setNames(as.list(1:3), c("x", "y", "z")))`
Line 671: Change `#' t <- tree_from(letters[1:4])` to `#' t <- as_flexseq(letters[1:4])`
Line 673: Change `#' tn <- tree_from(setNames(as.list(1:3), c("a", "b", "c")))` to `#' tn <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))`

### 6.8 `R/empty_tree.R` (@keywords internal — wrap examples in `\dontrun{}`)

Replace the `@examples` block (lines 5-11):

**Before:**
```r
#' @examples
#' t <- empty_tree()
#' t
#'
#' count_m <- MeasureMonoid(`+`, 0, function(el) 1)
#' t2 <- empty_tree(monoids = list(count = count_m))
#' attr(t2, "measures")
```

**After:**
```r
#' @examples
#' \dontrun{
#' t <- empty_tree()
#' t
#'
#' count_m <- measure_monoid(`+`, 0, function(el) 1)
#' t2 <- empty_tree(monoids = list(count = count_m))
#' attr(t2, "measures")
#' }
```

Also update the `@param` description on line 3:
Change: ``@param monoids Optional named list of `MeasureMonoid` objects.``
To: ``@param monoids Optional named list of `measure_monoid` objects.``

### 6.9 `R/concat_trees.R` (@keywords internal — wrap examples in `\dontrun{}`)

Replace the `@examples` block (lines 9-21):

**Before:**
```r
#' @examples
#' left <- tree_from(letters[1:3])
#' right <- tree_from(letters[4:6])
#' t <- concat_trees(left, right)
#' cat_m <- MeasureMonoid(paste0, "", as.character)
#' fold_left(t, cat_m)
#'
#' # Concatenate trees carrying a custom monoid
#' sum_m <- MeasureMonoid(`+`, 0, as.numeric)
#' a <- tree_from(1:3, monoids = list(sum = sum_m))
#' b <- tree_from(4:5, monoids = list(sum = sum_m))
#' t2 <- suppressWarnings(concat_trees(a, b))
#' attr(t2, "measures")$sum
```

**After:**
```r
#' @examples
#' \dontrun{
#' left <- as_flexseq(letters[1:3])
#' right <- as_flexseq(letters[4:6])
#' t <- concat_trees(left, right)
#' cat_m <- measure_monoid(paste0, "", as.character)
#' fold_left(t, cat_m)
#' }
```

### 6.10 `R/tree_from.R` (@keywords internal — wrap examples in `\dontrun{}`)

Replace the `@examples` block (lines 8-18):

**Before:**
```r
#' @examples
#' t <- tree_from(letters[1:4])
#' t[[2]]
#'
#' sum_m <- measure_monoid(`+`, 0, as.numeric)
#' t2 <- tree_from(1:5, monoids = list(sum = sum_m))
#' attr(t2, "measures")
#'
#' # Named elements support character indexing
#' tn <- tree_from(setNames(as.list(letters[1:3]), c("k1", "k2", "k3")))
#' tn[["k2"]]
```

**After:**
```r
#' @examples
#' \dontrun{
#' t <- tree_from(letters[1:4])
#' t[[2]]
#' }
```

### 6.11 `R/utils.R` — `get_graph_df` and `plot_tree` (@keywords internal)

**`get_graph_df` examples (lines 7-15):**

Replace:
```r
#' @examples
#' t <- tree_from(letters[1:4])
#' gdf <- get_graph_df(t)
#' names(gdf)
#' nrow(gdf[[1]])
#'
#' # Works for deeper trees too
#' t2 <- tree_from(letters[1:10])
#' gdf2 <- get_graph_df(t2)
#' nrow(gdf2[[2]])
```

With:
```r
#' @examples
#' \dontrun{
#' t <- as_flexseq(letters[1:4])
#' gdf <- get_graph_df(t)
#' names(gdf)
#' }
```

**`plot_tree` examples (lines 115-122):**

These are already wrapped in `\dontrun{}`. Update `tree_from` references:

Line 117: Change `#' t <- tree_from(letters[1:8])` to `#' t <- as_flexseq(letters[1:8])`
Line 120: Change `#' t2 <- tree_from(letters[1:12])` to `#' t2 <- as_flexseq(letters[1:12])`

### 6.12 Verify `R/flexseq.R`, `R/fold_left.R`, `R/fold_right.R`

These files already use `measure_monoid()` (lowercase) and `as_flexseq()` in their
examples. **No changes needed.**

### 6.13 Verify `R/priority_queue.R`, `R/print.priority_queue.R`, `R/Predicate.R`

These files already use correct exported functions in their examples.
**No changes needed.**

### 6.14 Verify `R/append.R` and `R/prepend.R`

`R/append.R` was fully rewritten in Phase 5.2 with correct examples.
`R/prepend.R` already uses `as_flexseq` in its examples. **No changes needed.**

---

## Phase 7: Clean up pkgdown config (Item 2)

### 7.1 Update `_pkgdown.yml`

**File:** `_pkgdown.yml`

The developer API section references functions that are `@keywords internal`
and not exported. These can still appear in pkgdown docs. However, remove
functions that won't have useful rendered pages. Update the reference section:

Replace the developer API section (lines 44-55):

**Before:**
```yaml
  - title: "developer API"
    desc: "Lower-level constructors, query primitives, and tooling"
    contents:
      - add_monoids
      - locate
      - split
      - split_tree
      - measure_monoid
      - predicate
      - validate_tree
      - validate_name_state
      - get_graph_df
```

**After:**
```yaml
  - title: "developer API"
    desc: "Lower-level constructors, query primitives, and tooling"
    contents:
      - add_monoids
      - locate
      - split_tree
      - measure_monoid
      - predicate
      - validate_tree
      - validate_name_state
```

Changes:
- Removed `split` (it's now `split.flexseq`, listed under flexseq API instead)
- Removed `get_graph_df` (internal, not useful to document externally)

Also update the flexseq API section to include `split.flexseq`. Add it after
the existing entries. Add this line after `"$<-.FingerTree"`:

```yaml
      - "split.flexseq"
```

---

## Phase 8: Create minimal vignette scaffolding (Item 14)

### 8.1 Update DESCRIPTION for vignettes

**File:** `DESCRIPTION`

Add `knitr` and `rmarkdown` to `Suggests` (after igraph, before pkgdown):

```
Suggests:
    igraph,
    knitr,
    pkgdown,
    rmarkdown,
    testthat (>= 3.0.0)
```

Add a new line after `Config/testthat/edition: 3`:

```
VignetteBuilder: knitr
```

### 8.2 Create `vignettes/flexseq.Rmd`

**Create file:** `vignettes/flexseq.Rmd`

```rmd
---
title: "Getting Started with flexseq"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Getting Started with flexseq}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(immutables)
```

## Creating sequences

`flexseq` objects are persistent (immutable) sequences. Every modification
returns a new sequence, leaving the original unchanged.

```{r}
x <- flexseq(1, 2, 3)
x

y <- as_flexseq(letters[1:5])
y
```

## Indexing

```{r}
y[[3]]
y[c(1, 3, 5)]
```

## Appending and prepending

```{r}
z <- append(y, "f")
z2 <- prepend(z, "start")
length(z2)
```

## Named sequences

```{r}
named <- as_flexseq(setNames(as.list(1:3), c("a", "b", "c")))
named[["b"]]
named$c
```

## Concatenation

```{r}
combined <- c(x, as_flexseq(4:6))
length(combined)
```

## Folding

```{r}
sum_m <- measure_monoid(`+`, 0, as.numeric)
fold_left(as_flexseq(1:10), sum_m)
```
```

### 8.3 Create `vignettes/priority-queues.Rmd`

**Create file:** `vignettes/priority-queues.Rmd`

```rmd
---
title: "Priority Queues"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Priority Queues}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(immutables)
```

## Creating a priority queue

```{r}
q <- priority_queue("task_a", "task_b", "task_c", priorities = c(3, 1, 2))
q
```

## Peeking at extremes

```{r}
peek_min(q)
peek_max(q)
```

## Extracting elements

`extract_min()` and `extract_max()` return both the element and the updated
queue, since priority queues are persistent.

```{r}
result <- extract_min(q)
result$element
result$priority
result$queue
```

## Inserting elements

```{r}
q2 <- insert(q, "task_d", priority = 0)
peek_min(q2)
```

## Persistence

The original queue is unchanged after extraction or insertion:

```{r}
peek_min(q)
is_empty(q)
```
```

### 8.4 Add `vignettes/` to `.Rbuildignore`

No action needed — vignettes are a standard package component and should NOT
be in `.Rbuildignore`. R will build them correctly.

---

## Phase 9: Regenerate and verify

### 9.1 Run `roxygen2::roxygenise()`

This regenerates `NAMESPACE` and all `man/*.Rd` files from the updated roxygen
comments.

```r
roxygen2::roxygenise()
```

### 9.2 Verify NAMESPACE changes

After roxygenise, check that NAMESPACE contains:

- `S3method(split,flexseq)` (was `export(split)`)
- `export(append)` (the generic)
- `S3method(append,default)`
- `S3method(append,flexseq)`
- No `import(igraph)` line
- No `import(rlist)` line
- `import(lambda.r)` still present

### 9.3 Run `R CMD check`

```bash
R CMD build . && R CMD check immutables_*.tar.gz
```

Fix any remaining NOTEs or WARNINGs.

### 9.4 Run tests

```r
testthat::test_local()
```

All existing tests should pass without modification (the `split` and `append`
call syntax is unchanged). The one test description string changed in Phase 2.4
is cosmetic only.

---

## Summary of all files modified

| File | Action |
|------|--------|
| `DESCRIPTION` | Remove LazyData, add BugReports, move igraph to Suggests, remove rlist, add knitr/rmarkdown/VignetteBuilder |
| `.Rbuildignore` | Add `^meta$` |
| `R/fingertree_measured.R` | DELETE |
| `R/MeasureMonoid.R` | Remove `MeasureMonoid` alias function |
| `R/Predicate.R` | Remove `Predicate` alias function |
| `R/measured.R` | Change 2 `MeasureMonoid()` calls to `measure_monoid()` |
| `R/add_left.R` | Replace `list.prepend` with `c(list(el), d)` |
| `R/add_right.R` | Replace `list.append` with `c(d, list(el))` |
| `R/utils.R` | Replace `list.prepend`, add igraph guard, namespace-qualify igraph calls, update examples |
| `R/fingertree-package.R` | Remove `@import rlist` and `@import igraph` |
| `R/split.R` | Full rewrite: standalone → `split.flexseq` S3 method |
| `R/append.R` | Full rewrite: standalone → S3 generic + `append.default` + `append.flexseq` |
| `R/locate.R` | Update examples |
| `R/split_tree.R` | Update examples |
| `R/add_monoids.R` | Update param description and examples |
| `R/validate.R` | Update examples |
| `R/dollar.R` | Update examples |
| `R/indexing.R` | Update examples (many occurrences) |
| `R/empty_tree.R` | Wrap examples in `\dontrun{}`, update param description |
| `R/concat_trees.R` | Wrap examples in `\dontrun{}`, update function calls |
| `R/tree_from.R` | Wrap examples in `\dontrun{}` |
| `_pkgdown.yml` | Update reference sections |
| `tests/testthat/test-cpp-parity.R` | Update 1 test description string |
| `vignettes/flexseq.Rmd` | CREATE |
| `vignettes/priority-queues.Rmd` | CREATE |
| `NAMESPACE` | REGENERATED by roxygen2 (do not edit manually) |
| `man/*.Rd` | REGENERATED by roxygen2 (do not edit manually) |
