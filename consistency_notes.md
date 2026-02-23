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
| `c()` | blocked | blocked (via ordered inheritance) | Aligned | Error text currently mentions `ordered_sequence`. |
| `push_front`/`push_back`/`insert_at` | blocked | blocked (via ordered inheritance) | Aligned | Same guardrail; interval message text may mention ordered type. |
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
