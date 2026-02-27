# Indexing for Ordered Sequences

Read indexing returns \`ordered_sequence\` subsets while preserving key
order. Replacement indexing is blocked to prevent order-breaking writes.

## Usage

``` r
# S3 method for class 'ordered_sequence'
x[i, ...]

# S3 method for class 'ordered_sequence'
x[[i, ...]]

# S3 method for class 'ordered_sequence'
x[i] <- value

# S3 method for class 'ordered_sequence'
x[[i]] <- value

# S3 method for class 'ordered_sequence'
x$name

# S3 method for class 'ordered_sequence'
x$name <- value
```

## Arguments

- x:

  An \`ordered_sequence\`.

- i:

  Index input.

- ...:

  Unused.

- value:

  Replacement value (unsupported).

- name:

  Element name (for \`\$\` and \`\$\<-\`).

## Value

Read methods return ordered payload values/subsets; replacement forms
always error.
