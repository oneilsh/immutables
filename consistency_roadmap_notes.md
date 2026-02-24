# Consistency Roadmap Notes (Cross-Structure)

Last reviewed: 2026-02-24

Scope: broader cross-structure roadmap/performance topics that are not specific to the
`ordered_sequence` vs `interval_index` pairwise consistency matrix.

## Monoid API Exposure Notes (2026-02-23)

Context: we discussed whether to remove user-facing `monoids =` constructor parameters and push advanced monoid attachment to `add_monoids()`.

### Current Behavior

- Constructors support `monoids =` and can build with monoids in one pass, including C++ fast paths.
- `add_monoids()` is available, but currently acts as a full rebind/recompute pass over the built tree.
- `measure_monoid()` is a developer API; element shape differs by structure:
  - `flexseq`: raw payload element.
  - `priority_queue`: entry wrapper (`item`, `priority`).
  - `ordered_sequence`: entry wrapper (`item`, `key`).
  - `interval_index`: entry wrapper (`item`, `start`, `end`).

### Performance Finding (directional)

Routing construction through `as_*()` then `add_monoids()` instead of constructor-time `monoids =` has significant cost:

- R backend: roughly `~3.1x` to `~3.3x` slower in local checks.
- C++ backend batch builds: dramatically slower in local checks (orders of magnitude in small calibrated runs), because the one-pass C++ build advantage is lost and monoid recomputation is forced through the rebind pass.

Interpretation: replacing internal constructor-time monoid wiring with `add_monoids()` broadly would be a material regression, especially for C++ batch paths.

### Preferred Direction

1. Keep internal monoid-aware builders (fast path) for implementation and batch operations.
2. Simplify public constructors over time (hide/remove `monoids =` from user-facing entry points).
3. Keep `add_monoids()` as the advanced user/developer API, with clearer documentation of per-structure `measure(el)` contracts.
4. If constructor `monoids =` is removed publicly, do it as a surface/API change only; do not replace internal fast-path plumbing with two-step `add_monoids()` calls.

### Progress (2026-02-24)

- Public constructor signatures were simplified to remove user-facing `monoids =` for:
  - `flexseq()`, `as_flexseq()`
  - `ordered_sequence()`, `as_ordered_sequence()`
  - `interval_index()`, `as_interval_index()`
  - `priority_queue()`, `as_priority_queue()`
- Internal monoid-aware build paths were retained via private `.*_build` / `.as_*_build` helpers, and package internals now route through those helpers when monoids are needed at construction time.
- `add_monoids()` now restores the input subclass (`flexseq`, `ordered_sequence`, `interval_index`, `priority_queue`) after recompute so it is safe as the user-facing advanced monoid entry point.
- Tests/docs were migrated from constructor-time monoids to `add_monoids(...)` where needed.

## Scalar Comparable Keys/Priorities (2026-02-23)

Context: we discussed expanding `ordered_sequence` keys and `priority_queue` priorities beyond current primitive constraints, to allow any scalar type with valid ordering.

### Current Constraints

- `ordered_sequence` keys currently accept only scalar `numeric` / `character` / `logical`.
- `priority_queue` priorities currently accept only scalar non-missing `numeric`.
- `interval_index` already supports broader scalar endpoint types so long as `<` and `>` are valid (for example `Date`/`POSIXct`), with mixed-type objects rejected.

### Feasibility Assessment

- Expanding support is feasible.
- The practical model is:
  - keep C++ fast paths for primitive key/priority types;
  - use R comparator fallback for other scalar orderable types.
- This matches existing interval-index behavior and preserves performance where the backend can optimize.

### Suggested Contract (if implemented)

1. Values must be scalar and non-missing.
2. Values must support deterministic scalar `<` and `>` comparisons.
3. Types must be homogeneous within an object (no mixed key/priority domains).
4. Equality semantics should be derived from comparator outcomes (`a < b`, `a > b`, otherwise equal), not from class-specific ad hoc checks.

### Implementation Notes

- `ordered_sequence`:
  - Generalize key normalization/type tagging to permit non-primitive scalar orderable types.
  - Keep existing C++ OMS insert path only for primitive key types; fallback to R split/compare path otherwise.
- `priority_queue`:
  - Replace numeric-only priority validator with scalar orderability validator.
  - Generalize `.pq_min` / `.pq_max` monoid compare logic away from numeric-only assumptions.
  - Maintain tie stability (FIFO within equal-priority runs).

### Risk Notes

- Comparator edge cases (`NA`, class-specific Ops behavior, mixed classes) need explicit validation and tests.
- Performance for non-primitive types will be R-fallback and should be documented as such.
