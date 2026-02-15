# Architecture

## Design Contract

This package follows a two-track implementation model:

1. Reference semantics in R (lambda.r style) are authoritative.
2. Fast paths (pure R helpers and optional C++ wrappers) are accelerators, not
   semantic forks.

If a fast backend diverges from reference behavior, the reference behavior is
correct and tests must be updated to enforce parity.

Primary algorithm reference:

- Hinze, R. and Paterson, R. (2006), *Finger trees: a simple general-purpose
  data structure*.
  <https://www.cs.ox.ac.uk/ralf.hinze/publications/FingerTrees.pdf>

## Backend Dispatch Policy

- Exported API code performs backend gating explicitly.
- C++ fast path is used only when enabled and monoids are eligible.
- Otherwise, pure-R fast helpers are used.
- Reference lambda.r internals remain readable and independent of C++ wrappers.

Backend gate functions and C++ wrappers live in `R/20-backend-cpp.R`.

## File Families

- `R/00-core-ref-*.R`: reference core internals (lambda.r style).
- `R/10-core-fast-r-*.R`: pure-R fast helpers (`*_fast`, hot-path helpers).
- `R/20-backend-cpp-*.R`: `.Call` wrappers and backend capability checks.
- `R/30-api-flexseq-*.R`: flexseq-facing API and supporting wrappers.
- `R/40-priority_queue-*.R`: priority queue API and print/monoid helpers.
- `R/90-devtools-*.R`: internal developer/debug helpers.

## Extension Guide

When adding a new structure family:

1. Keep shared finger-tree semantics in `00-core-ref-*`.
2. Add optional pure-R accelerators in `10-core-fast-r-*` only when profiling
   justifies them.
3. Keep optional C++ wrappers isolated in `20-backend-cpp-*`.
4. Put user-facing structure API in `40-<structure>-*.R`.
5. Add/extend parity tests so fast paths and reference behavior stay aligned.

## Validation Expectations

- `testthat::test_local()` passes.
- Backend parity scenarios remain green.
- `R CMD check` remains clean (or only intentional, documented notes).
