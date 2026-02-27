# Get interval bounds in sequence order

Get interval bounds in sequence order

## Usage

``` r
interval_bounds(x)
```

## Arguments

- x:

  An \`interval_index\`.

## Value

A data frame in current sequence order with one row per entry and two
list-columns:

- \`start\`:

  Start endpoint for each entry.

- \`end\`:

  End endpoint for each entry.

Returns a zero-row data frame with the same columns for empty indexes.
