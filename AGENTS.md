# AGENTS.md

## Purpose
Use this file as the durable startup context for work in this repository.
Treat tests and current source as the source of truth when docs conflict.

## Project Snapshot
- Package: `immutables` (R package using finger trees with optional C++ fast paths).
- Core structures:
  - `flexseq`: general persistent sequence.
  - `priority_queue`: queue-first API layered on `flexseq`.
  - `ordered_sequence`: key-ordered sequence API.
- Current API direction (important):
  - `push_front/push_back/pop_front/pop_back/peek_front/peek_back` are the preferred flexseq end-ops.
  - `priority_queue` intentionally blocks most sequence-style mutation/traversal; cast with `as_flexseq()` for those.
  - Ordered types block order-breaking writes and block `c()`.
  - Ordered merge was removed (for now); do not assume `merge.ordered_sequence`.
  - `apply` is an S3 generic across base/flexseq/priority_queue/ordered_sequence.

## Recent Branch Trajectory (api-align)
Useful commit landmarks for intent:
- `b97a2b5` API alignment work begins (`apply` generic direction, shared method patterns).
- `3009b10` generalized reuse of flexseq infrastructure for subclasses.
- `0a5a601` ordered types split into `ordered_sequence` + `ordered_multiset` (now removed).
- `7b6fced` priority queue surface narrowed (queue-first UX).
- `ffcb101` removed fold/walk machinery from public surface.
- `c07b4e6` migrated user-facing sequence endpoints to push/pop/peek names.
- `b276f73` removed ordered merge methods for now.
- `e920165` minor pkgdown reference reorder.

Interpretation:
- The codebase is intentionally simplifying and narrowing APIs.
- If in doubt, follow current tests + `NAMESPACE` over historical docs/blog-style examples.

## First Files to Read
1. `NAMESPACE` (what is exported and dispatched right now).
2. `R/40-ordered_sequence-core.R`
3. `R/40-priority_queue-core.R`
4. `tests/testthat/test-ordered-sequence.R`
5. `tests/testthat/test-priority-queue.R`
6. `tests/testthat/test-cpp-parity.R`
7. `tests/testthat/test-cpp-gc-safety.R`
8. `meta/bench_runner.R`

## Ground Rules
- Preserve persistence semantics (never mutate user-visible structures in place).
- Keep R reference behavior authoritative; C++/fast paths must match parity.
- Avoid introducing API aliases unless explicitly requested.
- Keep ordered semantics deterministic:
  - `lower_bound` is first `>= key`
  - `upper_bound` is first `> key`
  - duplicate-key operations are leftmost/FIFO within equal-key runs.
- For `priority_queue`, keep queue-oriented UX and strict name-based reads.
- This software is pre-release, there is no need for backward compatility

## Standard Commands
- Load package:
  - `R -q -e "devtools::load_all(quiet=TRUE)"`
- Full tests:
  - `R -q -e "devtools::test()"`
- Targeted tests:
  - `R -q -e "devtools::test(test_file='test-ordered-sequence.R')"`
  - `R -q -e "devtools::test(test_file='test-priority-queue.R')"`
- C++ GC stress tests (gated):
  - `R -q -e "Sys.setenv(IMMUTABLES_GC_STRESS='1'); devtools::test(test_file='test-cpp-gc-safety.R')"`
- Regenerate docs/NAMESPACE:
  - `R -q -e "devtools::document(quiet=TRUE)"`

## Benchmarks
- Preferred entrypoint: repo skill `immutables-bench`
  - Skill file:
    - `.agents/skills/immutables-bench/SKILL.md`
  - Use the skill workflow first for consistency of runs/notes/comparisons.
- Primary benchmark harness: `meta/bench_runner.R`
  - Direct commands (fallback if not using skill):
  - Quick profile:
    - `R -q -e "source('meta/bench_runner.R'); run_quick(note='...', use_cpp=TRUE)"`
  - Full profile:
    - `R -q -e "source('meta/bench_runner.R'); run_full(note='...', use_cpp=TRUE)"`
  - Scenario catalog:
    - `R -q -e "source('meta/bench_runner.R'); list_scenarios(base='quick')"`
  - Custom mix-and-match:
    - `R -q -e "source('meta/bench_runner.R'); run_scenarios(c('as_flexseq_only','ordered_sequence_insert'), note='...', use_cpp=TRUE)"`
  - Compare recent runs:
    - `R -q -e "source('meta/bench_runner.R'); compare_last(n=2)"`
- Benchmark outputs are local-only and gitignored:
  - `meta/bench-results/*.csv`
  - `meta/bench-results/*.meta.json`
- Legacy script still exists for ad-hoc reference:
  - `meta/bench.R`

## Common Pitfalls
- README/pkgdown may lag behind current API changes; verify against tests and `NAMESPACE`.
- Ordered and PQ class restoration behavior matters after operations; tests should assert class stacks.
- GC torture is time consuming; use targeted files and env-gated paths.
- When changing C++ wrappers or behavior, update both parity and GC safety coverage.
- High-churn areas (re-check tests after edits):
  - `R/30-api-flexseq-indexing.R`
  - `R/40-priority_queue-core.R`
  - `R/40-ordered_sequence-core.R`
  - `_pkgdown.yml` + generated man/NAMESPACE surface

## Change Checklist
For API or behavior changes:
1. Update relevant R implementation files.
2. Update/extend tests (unit + parity/GC where applicable).
3. Run targeted tests first, then `devtools::test()`.
4. Run `devtools::document(quiet=TRUE)` if roxygen-exposed behavior changed.
5. Update `_pkgdown.yml` reference entries when new/removed topics affect pkgdown.
6. Update this file and skills or benchmarks as needed.

## Updating AGENTS.md With Notes
You may add notes about the repo to the Notes section below in this file, located
at <project root>/AGENTS.md

You should also keep the information in this file accurate as the state of the
API or underlying implementations change.

## Notes
- Notes should be added as markdown list items.
- You may use sublists to organize notes
  - like this
  - and this
- Keep notes short and concrete (commit SHA + impact + risk).
- `eb493fd`: ordered_sequence key APIs aligned around `key`/`from_key`/`to_key`, `extract_key` renamed to `pop_key`, and missing-key reads are non-throwing (`peek_key(..., if_missing=...)`, `pop_key()` null-returns on miss). Risk: callers relying on previous argument names/error behavior need updates.
- `eb493fd`: removed `delete_one`/`delete_all`; `peek_key()` and `pop_key()` now support `which = "first"|"all"` where `"all"` operates on full duplicate-key runs and returns an ordered slice. Risk: code using deleted functions or assuming `pop_key()` always pops one element must migrate.
- `eb493fd`: `pop_front()`/`pop_back()` now return `element` (not `value`) plus `rest`; tests/docs/vignette updated. Risk: callers using `$value` must switch to `$element`.
- `eb493fd`: removed `ordered_multiset` API/tests and dropped package-level set-op generics (`union`/`intersect`/`setdiff`) tied to that surface. Risk: any code importing those package methods must migrate to explicit sequence operations.
- `eb493fd`: hardened C++ monoid/measures field access under GC pressure in `src/ft_cpp.cpp` (name lookup fallback for monoid subfields; recursive fallback for `.size`/`.named_count` reads when measure names are missing). Risk: GC torture remains expensive; `pop_key(which='all')` is excluded from GC-stress assertions to keep the suite deterministic.
- `eb493fd`: `ensure_size_monoids()` now canonicalizes every monoid spec to named `list(f, i, measure)` and validates that `f`/`measure` are functions before use. Impact: C++ positional fallback remains safe even if monoid list names are absent; risk: malformed custom `MeasureMonoid` objects now fail earlier with clearer errors.
- `eb493fd`: benchmark harness no longer references removed `ordered_multiset`/set-op scenarios; quick/full profiles now target live APIs only. Risk: historical OMS benchmark comparisons are no longer directly reproducible from default profiles.
- `eb493fd`: quick/full benchmark profiles now include `ordered_sequence_insert` to keep ordered-keyed workload coverage after OMS removal; `pq_insert_pop` remains available as a custom-only scenario. Risk: historical quick/full trend lines have one scenario changed.
- `eb493fd`: full-profile benchmark sizes were reduced in high-cost R-backend scenarios (`flexseq_tree_from*`, split variants, index reads, `as_flexseq_only`) to keep single-scenario runs within ~30s when capped. Risk: full-profile values are less stress-heavy than earlier settings.
- `eb493fd`: fixed `insert.ordered_sequence()` R fallback boundary case (`use_cpp=FALSE`) by avoiding public `push_back()` guard when appending split-left fragments; `ordered_sequence_insert` benchmarks now execute on R backend. Risk: internal append helper path should continue to preserve ordered wrapping invariants (covered by ordered-sequence tests).
- `eb493fd`: benchmark coverage now includes additional non-flexseq structure scenarios (`ordered_sequence_bounds`, `pq_insert_pop`) and both are in quick/full profiles with R-safe parameter sizes. Risk: scenario names/default params changed; compare historical runs by scenario+params, not just run label.
- `eb493fd`: added deeper ordered-sequence and priority-queue benchmark scenarios (`ordered_sequence_range_queries`, `ordered_sequence_pop_cycle`, `pq_peek_min_max`, `pq_pop_min_drain`, `pq_mixed_ops`) to improve optimization signal across lookup vs mutation-heavy paths. Risk: broader quick/full profiles increase total run time; use `run_scenarios()` for focused profiling.
