# Refactor Plan (High-Level)

## Goal
Reorganize the package around a clear two-track implementation model:
1. Reference implementation in R using `lambda.r` syntax for core finger-tree semantics.
2. Fast implementation using C++ where performance-critical operations benefit.

The exported user API should remain stable unless a change is explicitly required.

Reference basis:
- Hinze, R. and Paterson, R. (2006), *Finger trees: a simple general-purpose data structure*.
  https://www.cs.ox.ac.uk/ralf.hinze/publications/FingerTrees.pdf

## Design Principles
- Reference semantics are authoritative: R/lambda.r behavior defines correctness.
- C++ is an optional acceleration layer, never a semantic fork.
- Not every helper used by fast paths must be re-implemented in C++.
- Keep backend parity testing as a core contract.
- Keep structure easy to extend for future data structures.
- Keep all work CRAN-safe (checks, examples, docs, namespace hygiene).

## Target Code Organization
Use file families in `R/` by layer and responsibility:
- `00-core-ref-*.R`: reference finger-tree internals (lambda.r style).
- `10-core-fast-r-*.R`: pure-R fast helpers (`*_fast`, non-lambda where appropriate).
- `20-backend-cpp-*.R`: `.Call` wrappers, backend gating, C++ integration.
- `30-api-flexseq-*.R`: exported flexseq-facing API and wrappers.
- `40-priority_queue-*.R`: priority queue user-facing layer.
- `90-devtools-*.R`: internal validation/plot/debug/developer-only helpers.

This leaves room for future families like `40-<new_structure>-*.R` without mixing concerns.

## Scope
In scope:
- Reorganize files/functions into the target layering.
- Clarify backend dispatch boundaries.
- Preserve semantics and public API where possible.
- Update tests/docs to match organization and any necessary API adjustments.

Out of scope for this phase:
- Large semantic redesigns.
- New user-facing structures.
- Broad algorithmic changes unrelated to organization.

## High-Level Phases
1. Baseline and guardrails
- Snapshot current behavior with full tests and parity tests.
- Record current check status (`R CMD check`).

2. Architecture skeleton
- Create/confirm naming conventions and file-family boundaries.
- Add a brief architecture note for contributors.
- Ensure the Hinze/Paterson paper is cited in architecture docs and remains
  visibly referenced in package documentation metadata (e.g., DESCRIPTION/README).

3. Mechanical reorganization
- Move code into the target file families with minimal behavior changes.
- Keep function names stable unless necessary.

4. Backend boundary cleanup
- Ensure each accelerated operation has a clear dispatch flow:
  - C++ path when eligible,
  - otherwise fast-R/reference fallback.
- Keep reference logic independent and readable.

5. Test reorganization and updates
- Keep/expand backend parity tests for accelerated operations.
- Ensure API tests remain backend-agnostic.
- Add missing tests if API changes are required.

6. Documentation and CRAN hardening
- Regenerate docs and verify examples.
- Ensure CRAN check cleanliness and package structure compliance.

## Validation Criteria
- Public API behavior remains unchanged unless intentionally revised.
- C++ and reference R outputs match on parity coverage.
- `testthat::test_local()` passes.
- `R CMD check` is clean (or only intentionally accepted notes).
- File layout reflects the target layer model and is easier to navigate.

## Risks and Mitigations
- Risk: accidental semantic drift during moves.
  - Mitigation: phase-by-phase moves + parity tests after each phase.
- Risk: dispatch regressions across backends.
  - Mitigation: explicit backend gate tests and parity coverage checks.
- Risk: CRAN regressions from doc/example churn.
  - Mitigation: regenerate docs early and run check repeatedly.

## Deliverable of This Plan
A reorganized, extensible codebase where:
- core semantics are clearly located in lambda.r-based reference code,
- fast C++ paths are explicit and isolated,
- priority queue code is isolated under `40-priority_queue-*.R`,
- and parity/tests/checks enforce correctness and CRAN readiness.
