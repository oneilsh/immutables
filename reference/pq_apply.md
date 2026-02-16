# Apply a Function Over Priority Queue Entries

Applies a per-entry transform and rebuilds a \`priority_queue\`.

## Usage

``` r
pq_apply(q, f, reset_ties = TRUE, ...)
```

## Arguments

- q:

  A \`priority_queue\`.

- f:

  Callback called as \`f(item, priority, seq_id, name, ...)\`. It must
  return a named list using any subset of: \`item\`, \`priority\`,
  \`name\`. Missing fields inherit original values.

- reset_ties:

  Logical; if \`TRUE\`, refreshes tie-break \`seq_id\` by current order.
  If \`FALSE\`, preserves existing \`seq_id\` values.

- ...:

  Additional arguments passed to \`f\`.

## Value

A rebuilt \`priority_queue\`.

## Examples

``` r
q <- priority_queue("a", "bb", "ccc", priorities = c(1, 3, 2))
q2 <- pq_apply(q, function(item, priority, seq_id, name) {
  list(item = toupper(item))
})
peek_min(q2)
#> [1] "A"

q3 <- pq_apply(q, function(item, priority, seq_id, name) {
  list(priority = priority + nchar(item))
})
peek_max(q3)
#> [1] "bb"
```
