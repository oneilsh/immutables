# Pop elements for one key

For \`which = "first"\`, removes and returns the first (leftmost)
sequence element among entries whose key equals \`key\`.

## Usage

``` r
pop_key(x, key, which = c("first", "all"))
```

## Arguments

- x:

  An \`ordered_sequence\`.

- key:

  Query key.

- which:

  One of \`"first"\` or \`"all"\`.

## Value

A named list with components `element`, `key`, and `remaining`.

- For `which = "first"`:

  - On match: `element` is the first matching item and `key` is its key.

  - On miss: `element = NULL`, `key = NULL`, `remaining = x`.

- For `which = "all"`:

  - `element` is an `ordered_sequence` of all matching items in stable
    order. It may have size 0 (miss), 1 (single match), or greater than
    1 (multiple matches).

  - `key` is the normalized key on match, otherwise `NULL`.

  - `remaining` is the original sequence with that full key-run removed
    (or unchanged on miss).

## Details

For \`which = "all"\`, removes and returns all matching entries as an
ordered sequence.
