## Scalar Comparable Keys/Priorities

### TODO

1. Expand `ordered_sequence` key support beyond primitive numeric/character/logical to any scalar, non-missing, orderable type.
2. Expand `priority_queue` priority support beyond numeric to any scalar, non-missing, orderable type.
3. Enforce homogeneous key/priority type domains per object.
4. Keep C++ fast paths for supported primitive types and R fallback comparator paths for other scalar orderable types.
5. Define/implement comparator-driven equality (`<`, `>`, else equal) and add focused tests for:
   - `Date` / `POSIXct`
   - mixed-type rejection
   - NA handling
   - tie stability (FIFO within equal-key/equal-priority runs)
6. Add benchmark scenarios for non-primitive key/priority domains and document expected fallback costs.
