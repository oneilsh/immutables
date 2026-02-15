# Refactor Plan (Detailed)

This plan operationalizes `PLAN_HIGH_LEVEL.md` for a naive implementation agent.

Primary objective:
- Reorganize code around:
  1. reference finger-tree implementation in R (lambda.r style), and
  2. optional C++ acceleration path for speed-critical operations.

Reference basis:
- Hinze, R. and Paterson, R. (2006), *Finger trees: a simple general-purpose data structure*.
  https://www.cs.ox.ac.uk/ralf.hinze/publications/FingerTrees.pdf

Non-objectives for this plan:
- No intentional behavior changes.
- No broad API redesign.
- No new data structures.

## Hard Constraints
1. Preserve exported API semantics unless explicitly approved by user.
2. Keep C++/R backend parity contract intact (`tests/testthat/test-cpp-parity.R`).
3. Keep reference semantics in R as source of truth.
4. Keep CRAN cleanliness (`R CMD check`) and docs integrity (`roxygen2`, pkgdown).
5. Do not commit build artifacts (`immutables.Rcheck`, tarballs).

## Preflight Checklist (must pass before refactor)
1. Confirm current status and collect baseline:
- `git status --short`
- `R -q -e "testthat::test_local()"`
- `R -q -e "roxygen2::roxygenise()"`
- `R CMD build .`
- `R CMD check --no-manual immutables_*.tar.gz`
2. Record baseline outcomes in a short note in commit message or PR body.
3. If unrelated dirty changes exist, do not revert them; isolate your edits.

## Current Source Inventory (as of this plan)
Core/internal mixed today:
- `R/00-helpers.R`
- `R/constructors.R`
- `R/measured.R`
- `R/monoid_resolution.R`
- `R/view.R`
- `R/split_digit.R`
- `R/split_tree_impl.R`
- `R/locate_impl.R`
- `R/add_left.R`
- `R/add_right.R`
- `R/concat.R`
- `R/cpp_backend.R`
- `R/utils.R`

User-facing/API-adjacent today:
- `R/flexseq.R`, `R/append.R`, `R/prepend.R`
- `R/fold_left.R`, `R/fold_right.R`
- `R/split.R`, `R/split_tree.R`, `R/locate.R`
- `R/indexing.R`, `R/dollar.R`
- `R/add_monoids.R`, `R/validate.R`
- `R/MeasureMonoid.R`, `R/Predicate.R`
- `R/concat_trees.R`, `R/tree_from.R`, `R/empty_tree.R`
- `R/print.FingerTree.R`, `R/print.priority_queue.R`
- `R/priority_queue.R`, `R/pq_monoids.R`

## Target File Layout
Create/rename to these families:

### 00-core-ref-*.R (reference lambda.r internals)
- `R/00-core-ref-helpers.R`
- `R/00-core-ref-constructors.R`
- `R/00-core-ref-measured.R`
- `R/00-core-ref-monoid-resolution.R`
- `R/00-core-ref-view.R`
- `R/00-core-ref-split-digit.R`
- `R/00-core-ref-split-tree-impl.R`
- `R/00-core-ref-locate-impl.R`
- `R/00-core-ref-add-left.R`
- `R/00-core-ref-add-right.R`
- `R/00-core-ref-concat.R`

### 10-core-fast-r-*.R (pure R fast helpers)
- `R/10-core-fast-r-add-left.R`
- `R/10-core-fast-r-add-right-and-measure.R`
- `R/10-core-fast-r-concat.R`
- Keep `*_fast` and `*_impl` plain-R helpers here.

### 20-backend-cpp-*.R (C++ gateway)
- `R/20-backend-cpp.R` (from `R/cpp_backend.R`)

### 30-api-flexseq-*.R (exported flexseq API + wrappers)
- `R/30-api-flexseq-core.R` (from `R/flexseq.R`)
- `R/30-api-flexseq-measure-monoid.R` (from `R/MeasureMonoid.R`)
- `R/30-api-flexseq-predicate.R` (from `R/Predicate.R`)
- `R/30-api-flexseq-append.R` (from `R/append.R`)
- `R/30-api-flexseq-prepend.R` (from `R/prepend.R`)
- `R/30-api-flexseq-fold-left.R` (from `R/fold_left.R`)
- `R/30-api-flexseq-fold-right.R` (from `R/fold_right.R`)
- `R/30-api-flexseq-split.R` (from `R/split.R`)
- `R/30-api-flexseq-split-tree.R` (from `R/split_tree.R`)
- `R/30-api-flexseq-locate.R` (from `R/locate.R`)
- `R/30-api-flexseq-indexing.R` (from `R/indexing.R`)
- `R/30-api-flexseq-dollar.R` (from `R/dollar.R`)
- `R/30-api-flexseq-add-monoids.R` (from `R/add_monoids.R`)
- `R/30-api-flexseq-print.R` (from `R/print.FingerTree.R`)
- `R/30-api-flexseq-validate.R` (from `R/validate.R`)
- `R/30-api-flexseq-tree-from.R` (from `R/tree_from.R`, internal)
- `R/30-api-flexseq-empty-tree.R` (from `R/empty_tree.R`, internal)
- `R/30-api-flexseq-concat-trees.R` (from `R/concat_trees.R`, internal)

### 40-priority_queue-*.R
- `R/40-priority_queue-monoids.R` (from `R/pq_monoids.R`)
- `R/40-priority_queue-core.R` (from `R/priority_queue.R`)
- `R/40-priority_queue-print.R` (from `R/print.priority_queue.R`)

### 90-devtools-*.R (internal-only helpers)
- `R/90-devtools-utils.R` (from `R/utils.R`)

Keep as-is unless necessary:
- `R/fingertree-package.R`

## Migration Mapping Rules
1. Use `git mv` for one-to-one file renames.
2. For split files (`add_left.R`, `add_right.R`, `concat.R`):
- Move lambda.r/reference definitions into `00-core-ref-*` files.
- Move `*_fast` plain-R helpers into corresponding `10-core-fast-r-*` files.
3. Do not rename functions unless unavoidable.
4. Keep roxygen blocks attached to the same functions after moves.
5. Preserve `@keywords internal` and exported tags unchanged unless explicitly needed.

## Phase-by-Phase Execution

### Phase 1: Skeleton and Architecture Doc
1. Create `docs/architecture.md` with:
- reference-vs-fast design contract,
- backend dispatch policy,
- file-family map,
- paper citation,
- extension guide for future `40-<structure>-*.R` families.
2. Ensure paper citation also appears in package-level docs context:
- verify DESCRIPTION/README still clearly reference Hinze/Paterson.

Gate:
- `R -q -e "roxygen2::roxygenise()"` (no broken docs)

### Phase 2: Mechanical File Reorganization (No behavior changes)
1. Rename straightforward files with `git mv`.
2. Split mixed files:
- `add_left.R` into reference + fast-r files.
- `add_right.R` into reference + fast-r files.
- `concat.R` into reference + fast-r files.
3. Keep source code bodies unchanged except location and minimal comment edits.

Gate:
- `R -q -e "testthat::test_local()"`
- Must remain `FAIL 0`.

### Phase 3: Backend Boundary Clarification
1. Verify every accelerated wrapper in `R/20-backend-cpp.R` has clear fallback in R path.
2. Confirm dispatch sites in exported wrappers remain explicit and consistent:
- append/prepend/split/split_tree/locate/tree_from/concat_trees/indexing helpers.
3. Do not force C++ paths where currently intentionally not used.

Gate:
- `R -q -e "testthat::test_local()"`
- `R CMD check --no-manual immutables_*.tar.gz`

### Phase 4: Test Organization and Parity Hardening
1. Keep existing tests functional; optional file renames for clarity are allowed if behavior unchanged.
2. Ensure parity tests still cover accelerated operations:
- append/prepend/tree_from/concat/split_tree/split/locate/indexing/name lookup/get-by-index.
3. Add a small parity coverage assertion test if missing:
- static check that known `.ft_cpp_*` operations are represented by parity scenarios.
4. If any API shape changed, update tests and docs in same commit.

Gate:
- `R -q -e "testthat::test_local()"` with parity green.

### Phase 5: Documentation and Site Sync
1. Run `roxygen2::roxygenise()`.
2. Rebuild pkgdown site (`pkgdown::build_site()`) and ensure reference index remains valid.
3. Confirm docs still present user-facing `flexseq` methods (not stale pages).

Gate:
- `R -q -e "roxygen2::roxygenise()"`
- `R -q -e "pkgdown::build_site()"`

### Phase 6: CRAN-Focused Final Validation
1. Full local checks:
- `R -q -e "testthat::test_local()"`
- `R CMD build .`
- `R CMD check --no-manual immutables_*.tar.gz`
2. If LaTeX available, run full manual check too:
- `R CMD check immutables_*.tar.gz`
3. Ensure no accidental artifacts are staged:
- remove `immutables.Rcheck/` and tarballs before final commit if policy requires.

## Expected API Status After Refactor
Exported set should remain unchanged (from `NAMESPACE`):
- `add_monoids`, `append`, `as_flexseq`, `as_priority_queue`, `extract_max`,
  `extract_min`, `flexseq`, `fold_left`, `fold_right`, `insert`, `is_empty`,
  `locate`, `measure_monoid`, `peek_max`, `peek_min`, `predicate`, `prepend`,
  `priority_queue`, `split_tree`, `validate_name_state`, `validate_tree`
- plus S3 methods for `flexseq`, `append.default`, `append.flexseq`, `split.flexseq`.

If this set changes unintentionally, stop and fix before proceeding.

## CRAN Safety Checklist
1. No non-standard files accidentally included in tarball.
2. Examples remain runnable and concise.
3. Internal-only helpers remain `@keywords internal` where appropriate.
4. No undocumented exports.
5. `NAMESPACE` and `man/` regenerated from roxygen, not manually edited.
6. No platform-specific behavior introduced in tests/examples.

## Commit Strategy (Recommended)
1. Commit A: add architecture doc + no-op prep.
2. Commit B: mechanical file renames/moves only.
3. Commit C: split mixed files into 00/10 families.
4. Commit D: backend boundary clarifications + tests.
5. Commit E: docs/site/check cleanup.

This makes review and rollback safe.

## Completion Definition
All are true:
1. File layout follows 00/10/20/30/40/90 model.
2. Reference lambda.r core is clearly separated from fast-R and C++ layers.
3. Priority queue code is isolated under `40-priority_queue-*.R`.
4. Tests and parity pass.
5. `R CMD check` passes (or only user-approved environmental caveats).
6. Paper citation is present and visible in architecture/package documentation.
