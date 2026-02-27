# Indexing for Interval Indexes

Read indexing returns \`interval_index\` subsets while preserving
interval/key order. Replacement indexing is blocked.

## Usage

``` r
# S3 method for class 'interval_index'
x[i, ...]

# S3 method for class 'interval_index'
x[[i, ...]]

# S3 method for class 'interval_index'
x[i] <- value

# S3 method for class 'interval_index'
x[[i]] <- value

# S3 method for class 'interval_index'
x$name

# S3 method for class 'interval_index'
x$name <- value
```

## Arguments

- x:

  An \`interval_index\`.

- i:

  Index input.

- ...:

  Unused.

- value:

  Replacement value (unsupported).

- name:

  Element name (for \`\$\` and \`\$\<-\`).

## Value

Read methods return interval payload values/subsets; replacement forms
always error.

For \`\$\`: the matched payload element.

No return value; always errors because replacement indexing is
unsupported.
