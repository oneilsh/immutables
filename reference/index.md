# Package index

## Flexible Sequence (flexseq)

General-purpose persistent sequence with O(1) amortized push/pop at both
ends, O(log n) indexing and replacement, and efficient split/concat.

- [`flexseq()`](https://oneilsh.github.io/immutables/reference/flexseq.md)
  : Construct a Persistent Flexible Sequence
- [`as_flexseq()`](https://oneilsh.github.io/immutables/reference/as_flexseq.md)
  : Coerce to flexseq
- [`push_front()`](https://oneilsh.github.io/immutables/reference/push_front.md)
  : Push an element to the front
- [`push_back()`](https://oneilsh.github.io/immutables/reference/push_back.md)
  : Push an element to the back
- [`insert_at()`](https://oneilsh.github.io/immutables/reference/insert_at.md)
  : Insert elements at a position
- [`pop_front()`](https://oneilsh.github.io/immutables/reference/pop_front.md)
  : Pop the front element
- [`pop_back()`](https://oneilsh.github.io/immutables/reference/pop_back.md)
  : Pop the back element
- [`peek_front()`](https://oneilsh.github.io/immutables/reference/peek_front.md)
  : Peek at the front element
- [`peek_back()`](https://oneilsh.github.io/immutables/reference/peek_back.md)
  : Peek at the back element
- [`peek_at()`](https://oneilsh.github.io/immutables/reference/peek_at.md)
  : Peek at an element by position
- [`pop_at()`](https://oneilsh.github.io/immutables/reference/pop_at.md)
  : Pop an element by position
- [`split_at()`](https://oneilsh.github.io/immutables/reference/split_at.md)
  : Split by Scalar Index or Name
- [`c(`*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/c.flexseq.md)
  : Concatenate Sequences
- [`` `$`( ``*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.flexseq.md)
  [`` `$<-`( ``*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.flexseq.md)
  [`` `[`( ``*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.flexseq.md)
  [`` `[[`( ``*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.flexseq.md)
  [`` `[<-`( ``*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.flexseq.md)
  [`` `[[<-`( ``*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.flexseq.md)
  : Flexseq Indexing
- [`print(`*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/print.flexseq.md)
  : Print a flexseq
- [`length(`*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/length.flexseq.md)
  : Sequence Length
- [`as.list(`*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/as.list.flexseq.md)
  : Coerce a Sequence to Base List
- [`fapply(`*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/fapply.flexseq.md)
  : Apply a function over flexseq elements
- [`plot(`*`<flexseq>`*`)`](https://oneilsh.github.io/immutables/reference/plot.flexseq.md)
  : Plot a Sequence Tree

## Priority Queue

Persistent priority queue with O(log n) insert and min/max peek/pop.
Name-based read indexing only; cast with
[`as_flexseq()`](https://oneilsh.github.io/immutables/reference/as_flexseq.md)
for full sequence operations.

- [`priority_queue()`](https://oneilsh.github.io/immutables/reference/priority_queue.md)
  : Construct a Priority Queue
- [`as_priority_queue()`](https://oneilsh.github.io/immutables/reference/as_priority_queue.md)
  : Build a Priority Queue from elements and priorities
- [`insert(`*`<priority_queue>`*`)`](https://oneilsh.github.io/immutables/reference/insert.priority_queue.md)
  : Insert an element into a priority queue
- [`peek_min()`](https://oneilsh.github.io/immutables/reference/peek_min.md)
  : Peek minimum-priority element
- [`peek_max()`](https://oneilsh.github.io/immutables/reference/peek_max.md)
  : Peek maximum-priority element
- [`pop_min()`](https://oneilsh.github.io/immutables/reference/pop_min.md)
  : Pop minimum-priority element
- [`pop_max()`](https://oneilsh.github.io/immutables/reference/pop_max.md)
  : Pop maximum-priority element
- [`` `[`( ``*`<priority_queue>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.priority_queue.md)
  [`` `[[`( ``*`<priority_queue>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.priority_queue.md)
  [`` `[<-`( ``*`<priority_queue>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.priority_queue.md)
  [`` `[[<-`( ``*`<priority_queue>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.priority_queue.md)
  [`` `$`( ``*`<priority_queue>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.priority_queue.md)
  [`` `$<-`( ``*`<priority_queue>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.priority_queue.md)
  : Indexing for Priority Queues
- [`print(`*`<priority_queue>`*`)`](https://oneilsh.github.io/immutables/reference/print.priority_queue.md)
  : Print a Priority Queue
- [`length(`*`<priority_queue>`*`)`](https://oneilsh.github.io/immutables/reference/length.priority_queue.md)
  : Priority Queue Length
- [`fapply(`*`<priority_queue>`*`)`](https://oneilsh.github.io/immutables/reference/fapply.priority_queue.md)
  : Apply a function over priority queue entries
- [`plot(`*`<priority_queue>`*`)`](https://oneilsh.github.io/immutables/reference/plot.priority_queue.md)
  : Plot a Priority Queue Tree

## Ordered Sequence

Persistent key-ordered sequence with O(log n) insert, key lookup, and
range queries. Read indexing preserves key order; replacement indexing
is not supported.

- [`ordered_sequence()`](https://oneilsh.github.io/immutables/reference/ordered_sequence.md)
  : Construct an Ordered Sequence
- [`as_ordered_sequence()`](https://oneilsh.github.io/immutables/reference/as_ordered_sequence.md)
  : Build an Ordered Sequence from elements
- [`insert(`*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/insert.ordered_sequence.md)
  : Insert an element into an ordered sequence
- [`peek_key()`](https://oneilsh.github.io/immutables/reference/peek_key.md)
  : Peek elements for one key
- [`pop_key()`](https://oneilsh.github.io/immutables/reference/pop_key.md)
  : Pop elements for one key
- [`lower_bound()`](https://oneilsh.github.io/immutables/reference/lower_bound.md)
  : Find first element with key \>= value
- [`upper_bound()`](https://oneilsh.github.io/immutables/reference/upper_bound.md)
  : Find first element with key \> value
- [`elements_between()`](https://oneilsh.github.io/immutables/reference/elements_between.md)
  : Return elements in a key range
- [`count_key()`](https://oneilsh.github.io/immutables/reference/count_key.md)
  : Count elements matching one key
- [`count_between()`](https://oneilsh.github.io/immutables/reference/count_between.md)
  : Count elements in a key range
- [`` `[`( ``*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.ordered_sequence.md)
  [`` `[[`( ``*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.ordered_sequence.md)
  [`` `[<-`( ``*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.ordered_sequence.md)
  [`` `[[<-`( ``*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.ordered_sequence.md)
  [`` `$`( ``*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.ordered_sequence.md)
  [`` `$<-`( ``*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.ordered_sequence.md)
  : Indexing for Ordered Sequences
- [`print(`*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/print.ordered_sequence.md)
  : Print an ordered sequence summary
- [`length(`*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/length.ordered_sequence.md)
  : Ordered Sequence Length
- [`as.list(`*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/as.list.ordered_sequence.md)
  : Coerce Ordered Sequence to List
- [`fapply(`*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/fapply.ordered_sequence.md)
  : Apply a function over ordered sequence entries
- [`plot(`*`<ordered_sequence>`*`)`](https://oneilsh.github.io/immutables/reference/plot.ordered_sequence.md)
  : Plot an Ordered Sequence Tree

## Interval Index

Persistent interval index with O(log n) insertion and immutable interval
query/pop helpers over ordered interval endpoints.

- [`interval_index()`](https://oneilsh.github.io/immutables/reference/interval_index.md)
  : Construct an Interval Index
- [`as_interval_index()`](https://oneilsh.github.io/immutables/reference/as_interval_index.md)
  : Build an Interval Index from elements and interval bounds
- [`insert(`*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/insert.interval_index.md)
  : Insert an element into an interval index
- [`peek_point()`](https://oneilsh.github.io/immutables/reference/peek_point.md)
  : Peek intervals containing a point
- [`pop_point()`](https://oneilsh.github.io/immutables/reference/pop_point.md)
  : Pop intervals containing a point
- [`peek_overlaps()`](https://oneilsh.github.io/immutables/reference/peek_overlaps.md)
  : Peek intervals overlapping a query interval
- [`peek_containing()`](https://oneilsh.github.io/immutables/reference/peek_containing.md)
  : Peek intervals containing a query interval
- [`peek_within()`](https://oneilsh.github.io/immutables/reference/peek_within.md)
  : Peek intervals within a query interval
- [`pop_overlaps()`](https://oneilsh.github.io/immutables/reference/pop_overlaps.md)
  : Pop overlapping intervals
- [`pop_containing()`](https://oneilsh.github.io/immutables/reference/pop_containing.md)
  : Pop intervals containing a query interval
- [`pop_within()`](https://oneilsh.github.io/immutables/reference/pop_within.md)
  : Pop intervals within a query interval
- [`interval_bounds()`](https://oneilsh.github.io/immutables/reference/interval_bounds.md)
  : Get interval bounds in sequence order
- [`` `[`( ``*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.interval_index.md)
  [`` `[[`( ``*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.interval_index.md)
  [`` `[<-`( ``*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.interval_index.md)
  [`` `[[<-`( ``*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.interval_index.md)
  [`` `$`( ``*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.interval_index.md)
  [`` `$<-`( ``*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/sub-.interval_index.md)
  : Indexing for Interval Indexes
- [`print(`*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/print.interval_index.md)
  : Print an interval index summary
- [`length(`*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/length.interval_index.md)
  : Interval Index Length
- [`as.list(`*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/as.list.interval_index.md)
  : Coerce Interval Index to List
- [`fapply(`*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/fapply.interval_index.md)
  : Apply a function over interval index entries
- [`plot(`*`<interval_index>`*`)`](https://oneilsh.github.io/immutables/reference/plot.interval_index.md)
  : Plot an Interval Index Tree

## Developer Tools

Lower-level primitives for custom monoids, predicate queries, and
validation.

- [`insert()`](https://oneilsh.github.io/immutables/reference/insert.md)
  : Insert an element
- [`fapply()`](https://oneilsh.github.io/immutables/reference/fapply.md)
  : Fapply with S3 dispatch
- [`add_monoids()`](https://oneilsh.github.io/immutables/reference/add_monoids.md)
  : Add/merge monoids on an existing tree
- [`locate_by_predicate()`](https://oneilsh.github.io/immutables/reference/locate_by_predicate.md)
  : Locate First Predicate Flip Without Reconstructing Context Trees
- [`split_around_by_predicate()`](https://oneilsh.github.io/immutables/reference/split_around_by_predicate.md)
  : Split Around First Predicate Flip
- [`split_by_predicate()`](https://oneilsh.github.io/immutables/reference/split_by_predicate.md)
  : Split a flexseq into Left and Right Parts by Predicate
- [`measure_monoid()`](https://oneilsh.github.io/immutables/reference/measure_monoid.md)
  : Construct a Measure Monoid Specification
- [`predicate()`](https://oneilsh.github.io/immutables/reference/Predicate.md)
  : Construct a Predicate Function
- [`validate_tree()`](https://oneilsh.github.io/immutables/reference/validate_tree.md)
  : Validate full tree invariants (debug/test utility)
- [`validate_name_state()`](https://oneilsh.github.io/immutables/reference/validate_name_state.md)
  : Validate name-state invariants only (debug/test utility)
