# Consistency Notes: `ordered_sequence` vs `interval_index`

Last reviewed: 2026-02-23

Scope: user-facing API surface and semantics, based on current source/tests.

## Alignment Log

- 2026-02-20:
  - `fapply.ordered_sequence()` aligned to payload-only return (metadata read-only).
  - `fapply.priority_queue()` aligned to payload-only return (metadata read-only).
  - `fapply.flexseq()` now always preserves/recomputes existing monoids; removed `preserve_monoids` parameter.
  - Added explicit `as_flexseq.ordered_sequence()` and `as_flexseq.interval_index()` methods; subtype casts no longer fall through to failing default dispatch.
  - `ordered_sequence` and `priority_queue` `[[`/`$` reads now return payloads (not entry wrappers).
  - Current cross-structure state: all `fapply` methods now use payload-only returns.
- 2026-02-23:
  - `interval_index` relation reads now use `peek_overlaps` / `peek_containing` / `peek_within`; removed relation `find_*` APIs for these operations.
  - `interval_index` point APIs now use `peek_point` / `pop_point`; removed `find_point`.
  - `interval_index` query API hard-replaced from `find_*` to `peek_*` (no compatibility aliases).
  - `interval_index` now blocks `peek_front` / `peek_back` / `peek_at` and `pop_front` / `pop_back` / `pop_at`.
  - Ordered-subclass blocker errors are now class-generic for inherited APIs (`c`, `push_front`, `push_back`, `insert_at`, replacement indexing), so subclasses report their concrete class name in errors.
  - Removed legacy internal ordered merge primitive (`.ft_cpp_oms_set_merge`) from backend R/C++ bindings and from parity/GC test coverage; ordered merge remains intentionally unavailable until redesign.
  - Test consistency tightened: `validate_*` assertions now require invisible `TRUE`, redundant `fapply.flexseq` monoid-recompute case was collapsed, and mixed payload tests now assert value-level identity (not just types).

## `fapply` Cross-Structure Snapshot

| Type | `FUN` signature | `FUN` return | Metadata mutability | Monoid behavior |
|---|---|---|---|---|
| `flexseq` | `(item, ...)` | new payload item | N/A (no key/priority metadata) | Existing monoids are preserved and recomputed. |
| `priority_queue` | `(item, priority, name, ...)` | new payload item | `priority`/`name` are read-only | Existing queue/user monoids are preserved and recomputed. |
| `ordered_sequence` | `(item, key, name, ...)` | new payload item | `key`/`name` are read-only | Existing ordered/user monoids are preserved and recomputed. |
| `interval_index` | `(item, start, end, name, ...)` | new payload item | `start`/`end`/`name` are read-only | Existing interval/user monoids are preserved and recomputed. |

## Consistency Matrix

| Area | `ordered_sequence` | `interval_index` | Status | Notes / Follow-up |
|---|---|---|---|---|
| Class stack | `ordered_sequence -> flexseq` | `interval_index -> ordered_sequence -> flexseq` | Intentional | Shared internals with interval-first UX on top. |
| Construction keys/endpoints | `keys` (scalar, non-missing) | `start/end` (scalar, non-missing, `start <= end`) | Intentional | Different data model. |
| Allowed key/endpoint types | numeric/character/logical only | any scalar type with valid `<`/`>` (including `Date`/`POSIXct`) | Intentional (v1) | Interval index has broader endpoint domain. |
| Per-object boundary model | none (uses include flags per call in range APIs) | object default `bounds` (`[)`, `[]`, `()`, `(]`) + per-call override | Intentional | Interval semantics need explicit boundary token model. |
| Core read APIs | `lower_bound`, `upper_bound`, `peek_key`, `elements_between`, `count_*` | `peek_point`, `peek_overlaps`, `peek_containing`, `peek_within` | Intentional | Interval type blocks ordered-key APIs. |
| Core pop APIs | `pop_key(which='first'|'all')` | `pop_point` / `pop_overlaps` / `pop_containing` / `pop_within` (`which='first'|'all'`) | Intentional | Same immutable pop pattern, relation-specific API. |
| No-match read behavior | `peek_key()` errors | `peek_*(which='first')` returns `NULL`; `peek_*(which='all')` returns empty `interval_index` | Intentional | Interval peeks mirror ordered first/all shape with immutable slice returns for `all`. |
| `[` subsetting | strict increasing indices only; no reorder/dupes | same | Aligned | Same invariant enforcement. |
| Replacement indexing (`[<-`, `[[<-`, `$<-`) | blocked | blocked | Aligned | Order-breaking writes blocked for both. |
| `c()` | blocked | blocked (via ordered inheritance) | Aligned | Error text now reports the concrete ordered-subclass class name. |
| `push_front`/`push_back`/`insert_at` | blocked | blocked (via ordered inheritance) | Aligned | Same guardrail; inherited blocker text now reports concrete subclass names. |
| `[[` return shape | returns payload `item` | returns payload `item` | Aligned | `ordered_sequence` now has explicit payload-returning `[[` method. |
| `$` return shape | returns payload `item` by name | returns payload `item` by name | Aligned | `ordered_sequence` now has explicit payload-returning `$` method. |
| `peek_front` / `peek_back` / `peek_at` availability | supported | blocked | Intentional | Interval index is interval-query-first; use `peek_*`/`pop_*` helpers. |
| `pop_front` / `pop_back` / `pop_at` availability | supported | blocked | Intentional | Interval index uses relation-based immutable pops (`pop_point`, `pop_overlaps`, `pop_containing`, `pop_within`). |
| `fapply` contract | `FUN(item, key, name, ...)` returns new payload item; key/name read-only | `FUN(item, start, end, name, ...)` returns new payload item; metadata read-only | Aligned | Aligned on 2026-02-20. |
| Cast to `as_flexseq()` | dedicated method; drops ordered wrapper and `.oms_max_key` | dedicated method; drops interval wrapper and `.ivx_max_start`/`.oms_max_key` | Aligned | Both casts now return plain `flexseq` over stored entries. |
| Metadata accessor | no key-only accessor equivalent | `interval_bounds()` returns start/end | Intentional | Interval type has explicit interval metadata accessor. |
| Duplicate handling/stability | duplicates allowed; stable/FIFO within key runs | duplicate intervals allowed; stable/FIFO within equal-start runs | Aligned | Deterministic traversal order in both. |

## Immediate Alignment Candidates

1. None currently tracked for this pair (`ordered_sequence` vs `interval_index`).

## Decisions Locked

1. `priority_queue` remains strict name-only for `[[` reads (no positional `[[`).
2. `interval_index` uses interval-first query/pop APIs; sequence front/back/at endpoints remain blocked.
3. `interval_index` point queries use `peek_point`/`pop_point` with `which = "first"|"all"` semantics.
4. Ordered merge remains unavailable and there is currently no retained internal merge primitive.

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

## Ordered/Interval Error UX: Replacement Indexing (2026-02-23)

Observed issue:

- For ordered-like structures, replacement indexing is correctly blocked, but the current error target can leak internal structural classes.
- Example observed:
  - `` `[[<-()` is not supported for Deep. Replacement indexing is not supported. ``

Expected user-facing behavior:

- Error should reference the concrete public class (`ordered_sequence` or `interval_index`), never internal tree node classes (`Deep`, `Single`, `Empty`).
- Message should remain action-oriented (for ordered-like types, replacements are blocked to preserve order invariants).

Likely root cause:

- Ordered-like blocker helper derives target class from `class(x)` and currently permits structural classes through in some paths.
- The class selection logic should explicitly filter finger-tree implementation classes before choosing a displayed owner class.

Follow-up:

1. Tighten ordered-like owner-class resolution so blocker messages are stable and public-API oriented.
   - Status: implemented locally (post-2026-02-23 review).
   - Resolver now filters internal finger-tree classes (`Deep`, `Single`, `Empty`, `FingerTree`, `Digit`, `Node`) before selecting a displayed owner class.
   - Replacement-indexing blocker tests now assert public class names (`ordered_sequence`, `interval_index`) and assert that structural names are not leaked.
