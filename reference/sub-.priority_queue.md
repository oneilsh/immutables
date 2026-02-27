# Indexing for Priority Queues

Name-based indexing is supported for reads only. Positional indexing and
all replacement indexing are intentionally blocked to preserve
queue-first UX.

## Usage

``` r
# S3 method for class 'priority_queue'
x[i, ...]

# S3 method for class 'priority_queue'
x[[i, ...]]

# S3 method for class 'priority_queue'
x[i] <- value

# S3 method for class 'priority_queue'
x[[i]] <- value

# S3 method for class 'priority_queue'
x$name

# S3 method for class 'priority_queue'
x$name <- value
```

## Arguments

- x:

  A \`priority_queue\`.

- i:

  Index input. For reads, must be a character name (scalar for
  \`\[\[\`).

- ...:

  Unused.

- value:

  Replacement value (unsupported).

- name:

  Element name (for \`\$\` and \`\$\<-\`).

## Value

For \`\$\`/\`\[\[\`/\`\[\`: queue payload values or queue subsets by
name. Replacement forms always error.
