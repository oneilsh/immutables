# CRAN Cleanup Plan — High-Level

## 1. Fix all `@examples` blocks

**Problem:** Many examples reference non-exported functions (`MeasureMonoid()`, `tree_from()`,
`Predicate()`) causing `R CMD check` failures. The pkgdown site already shows these errors.

**Approach:**

- Replace `MeasureMonoid(...)` with `measure_monoid(...)` in all roxygen `@examples`.
- Replace `tree_from(...)` with `as_flexseq(...)` (the public equivalent).
- Replace `Predicate(...)` with `predicate(...)` if present in examples.
- Review all `@examples` in exported functions for simplicity and correctness.
  Use only exported, user-facing API calls. Examples should demonstrate canonical
  usage — straightforward and self-contained.
- For `@keywords internal` functions (`empty_tree`, `concat_trees`, `plot_tree`,
  `get_graph_df`): wrap examples in `\dontrun{}` since these aren't part of the
  public API, or replace with examples using exported functions where possible.
- After edits, run `roxygen2::roxygenise()` to regenerate `man/` pages and NAMESPACE.


## 2. Clean up developer API exports vs internals

**Problem:** Several exported S3 methods dispatch on `FingerTree` (an internal class),
and several `@keywords internal` functions appear in the pkgdown developer API section
despite not being exported. The public API should operate on `flexseq` / `priority_queue`
objects only.

**Approach:**

- `tree_from()`, `empty_tree()`, `concat_trees()` are already `@keywords internal`
  and not exported — no NAMESPACE changes needed. Ensure their examples use
  `\dontrun{}` or are removed.
- `get_graph_df()` and `plot_tree()` are `@keywords internal` — same treatment.
- The `_pkgdown.yml` developer API section references unexported functions.
  Remove them from pkgdown config or accept that pkgdown can document internal
  functions (they just won't be user-accessible without `:::`).
- S3 methods registered on `FingerTree` (`[.FingerTree`, `[[.FingerTree`, etc.)
  already have corresponding `flexseq` registrations in NAMESPACE — verify the
  flexseq methods dispatch correctly and that users always interact via `flexseq`.


## 3. Remove `LazyData: true`

**Problem:** DESCRIPTION has `LazyData: true` but no `data/` directory, producing an
`R CMD check` NOTE.

**Approach:** Remove the `LazyData: true` line from DESCRIPTION.


## 4. Resolve `split` and `append` base R masking

### `split` (clean solution available)

`base::split` is an S3 generic. Convert the current standalone `split()` export to a
proper `split.flexseq` S3 method.

**Current signature:** `split(t, predicate, monoid_name)`
**New signature:** `split.flexseq(x, f, ...)`

Where `x` is the flexseq tree, `f` is the predicate function, and `monoid_name` is
passed via `...` (positionally or as a named argument). The method is registered via
`@method split flexseq` / `S3method(split, flexseq)` in NAMESPACE.

Update call sites in tests: `split(t, pred, ".size")` continues to work unchanged
since R dispatches on the first argument's class.

### `append` (decided: S3 generic with default fallback)

`base::append` is NOT a generic — it's a plain function. We create our own S3
generic: `append <- function(x, ...) UseMethod("append")`, provide
`append.default` forwarding to `base::append(x, ...)`, and `append.flexseq`
wrapping the current implementation. This is a well-established CRAN pattern
(e.g. how `data.table` handles `merge`). Users get clean `append(t, x)`
semantics and `base::append()` still works on non-flexseq objects.

Update all tests and examples to reflect any signature changes.


## 5. Move `igraph` from Imports to Suggests

**Problem:** `igraph` (~5MB compiled) is used only in `plot_tree()`, which is
`@keywords internal`.

**Approach:**

- Move `igraph` from `Imports` to `Suggests` in DESCRIPTION.
- Remove `@import igraph` from `R/fingertree-package.R`.
- In `plot_tree()` (`R/utils.R`), add a `requireNamespace("igraph")` guard with
  a helpful error message if not installed.
- Qualify igraph function calls: `igraph::graph_from_data_frame()`,
  `igraph::layout_as_tree()`, `igraph::V()`, `igraph::vcount()`.
- Remove `import(igraph)` from NAMESPACE (will happen automatically via
  `roxygen2::roxygenise()`).


## 6. Eliminate `rlist` dependency

**Problem:** Only 3 usages of `rlist` functions (`list.prepend`, `list.append`),
used to prepend/append an element to a list while preserving flat structure.

**Approach:** Replace with base R equivalents that satisfy the stated requirements
(prepend/append element to list without nesting, then restore class attributes):

- `list.prepend(d, el)` → `c(list(el), d)` (in `R/add_left.R` and `R/utils.R`)
- `list.append(d, el)` → `c(d, list(el))` (in `R/add_right.R`)

The surrounding code already restores class attributes after these calls, so the
replacement is safe.

Remove `rlist` from DESCRIPTION Imports and `@import rlist` from
`R/fingertree-package.R`.


## 7. Add `meta/` to `.Rbuildignore`

**Problem:** Development files in `meta/` would be included in the CRAN tarball.

**Approach:** Add `^meta$` and `^meta/` patterns to `.Rbuildignore`.


## 8. README — deferred

User will work on README separately. No changes in this plan.


## 9. Add `BugReports` to DESCRIPTION

**Approach:** Add `BugReports: https://github.com/oneilsh/immutables/issues` to
DESCRIPTION.


## 10. `lambda.r` dependency — deferred

Deeply embedded (277 occurrences across 20 files). Removing it is a large refactor
that is out of scope for this round.


## 11. pkgdown title — already fixed by user

No action needed.


## 12. Remove `Predicate` (PascalCase alias)

**Problem:** `Predicate()` is an unexported alias for the exported `predicate()`.
Only referenced in one test (`test-cpp-parity.R:79`).

**Approach:**

- Remove the `Predicate` function definition from `R/Predicate.R` (keep `predicate`).
- Update the test in `test-cpp-parity.R` to use `predicate()` instead.
- Similarly, `MeasureMonoid` in `R/MeasureMonoid.R:25` is an unexported alias for
  `measure_monoid`. Remove the alias function. Update any internal code or tests
  that reference `MeasureMonoid()` to use `measure_monoid()`.


## 13. Remove placeholder file `R/fingertree_measured.R`

**Problem:** File contains only a TODO comment.

**Approach:** Delete `R/fingertree_measured.R`.


## 14. Create minimal vignette scaffolding

**Problem:** No vignettes exist. CRAN submission benefits from at least a
getting-started guide.

**Approach:**

- Create `vignettes/` directory.
- Create two minimal `.Rmd` vignettes:
  - `vignettes/flexseq.Rmd` — basic flexseq usage (construction, indexing,
    append/prepend, fold, concatenation).
  - `vignettes/priority-queues.Rmd` — basic priority queue usage (construction,
    insert, peek/extract min/max).
- Add `knitr` and `rmarkdown` to `Suggests` in DESCRIPTION.
- Add `VignetteBuilder: knitr` to DESCRIPTION.
- Content should be minimal/placeholder — user will flesh out later.


## Execution order

1. Items 3, 7, 9, 13 — trivial DESCRIPTION/.Rbuildignore fixes
2. Item 12 — remove PascalCase aliases
3. Item 6 — eliminate `rlist` dependency
4. Item 5 — move `igraph` to Suggests
5. Item 4 — convert `split`/`append` to proper S3 methods
6. Item 1 — fix all `@examples`
7. Item 2 — clean up pkgdown config and internal docs
8. Item 14 — create vignette scaffolding
9. Run `roxygen2::roxygenise()`, `R CMD check`, fix any remaining issues
