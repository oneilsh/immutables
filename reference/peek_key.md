# Peek elements for one key

For \`which = "first"\`, returns the first (leftmost) sequence element
among entries whose key equals \`key\`.

## Usage

``` r
peek_key(x, key, which = c("first", "all"))
```

## Arguments

- x:

  An \`ordered_sequence\`.

- key:

  Query key.

- which:

  One of \`"first"\` or \`"all"\`.

## Value

For \`which = "first"\`, raw stored element. For \`which = "all"\`, an
ordered sequence with matching elements. Throws when no matching key
exists.

## Details

For \`which = "all"\`, returns an ordered sequence containing exactly
the entries whose key equals \`key\`.
