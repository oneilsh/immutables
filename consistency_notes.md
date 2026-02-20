# Consistency Notes: `ordered_sequence` vs `interval_index`

Last reviewed: 2026-02-20

Scope: user-facing API surface and semantics, based on current source/tests.

## Alignment Log

- 2026-02-20:
  - `fapply.ordered_sequence()` aligned to payload-only return (metadata read-only).
  - `fapply.priority_queue()` aligned to payload-only return (metadata read-only).
  - `fapply.flexseq()` now always preserves/recomputes existing monoids; removed `preserve_monoids` parameter.
  - Current cross-structure state: all `fapply` methods now use payload-only returns.

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
| Core read APIs | `lower_bound`, `upper_bound`, `peek_key`, `elements_between`, `count_*` | `find_point`, `find_overlaps`, `find_containing`, `find_within` | Intentional | Interval type blocks ordered-key APIs. |
| Core pop APIs | `pop_key(which='first'|'all')` | `pop_overlaps` / `pop_containing` / `pop_within` (`which='first'|'all'`) | Intentional | Same immutable pop pattern, relation-specific API. |
| No-match read behavior | `peek_key()` errors | `find_*()` returns empty `interval_index` | Intentional | Different query families; both deterministic. |
| `[` subsetting | strict increasing indices only; no reorder/dupes | same | Aligned | Same invariant enforcement. |
| Replacement indexing (`[<-`, `[[<-`, `$<-`) | blocked | blocked | Aligned | Order-breaking writes blocked for both. |
| `c()` | blocked | blocked (via ordered inheritance) | Aligned | Error text currently mentions `ordered_sequence`. |
| `push_front`/`push_back`/`insert_at` | blocked | blocked (via ordered inheritance) | Aligned | Same guardrail; interval message text may mention ordered type. |
| `[[` return shape | returns stored entry (`list(item, key)`) via inherited `[[.flexseq` | returns payload `item` only | Alignment candidate | Observable UX mismatch for single-element reads. |
| `$` return shape | inherits `$.flexseq` -> same as `[[` (entry list) | custom `$.interval_index` -> payload only | Alignment candidate | Same underlying mismatch as `[[`. |
| `peek_front` / `peek_back` / `peek_at` return shape | payload item | payload item | Aligned | But differs from ordered `[[`/`$` currently. |
| `fapply` contract | `FUN(item, key, name, ...)` returns new payload item; key/name read-only | `FUN(item, start, end, name, ...)` returns new payload item; metadata read-only | Aligned | Aligned on 2026-02-20. |
| Cast to `as_flexseq()` | no dedicated method; default dispatch errors in current runtime | no dedicated method; default dispatch errors in current runtime | Likely bug | Repro seen locally: `as_flexseq(ordered_sequence(...))` and `as_flexseq(interval_index(...))` error. |
| Metadata accessor | no key-only accessor equivalent | `interval_bounds()` returns start/end | Intentional | Interval type has explicit interval metadata accessor. |
| Duplicate handling/stability | duplicates allowed; stable/FIFO within key runs | duplicate intervals allowed; stable/FIFO within equal-start runs | Aligned | Deterministic traversal order in both. |

## Immediate Alignment Candidates

1. Decide whether `ordered_sequence` single-element reads should return payloads (`item`) instead of raw entry lists for `[[`/`$`.
2. Add explicit `as_flexseq.ordered_sequence()` and `as_flexseq.interval_index()` methods (or intentionally document that casts are unsupported).

## Repro Notes (Current Runtime)

```r
devtools::load_all(quiet = TRUE)
xs <- ordered_sequence("a", "b", keys = c(2, 1))
as_flexseq(xs)   # currently errors

ix <- interval_index("x", "y", start = c(1, 2), end = c(3, 4))
as_flexseq(ix)   # currently errors
```
