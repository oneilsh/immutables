# Index a priority queue by name

Priority queues intentionally expose only name-based read indexing.

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
```

## Arguments

- x:

  A \`priority_queue\`.

- i:

  For \`\[\`, character names.

- ...:

  Unused.

- value:

  Replacement value (unsupported).

## Value

For \`\[\`, a \`priority_queue\` containing matched named entries.

For \`\[\[\`, one payload element matched by a single character name.

No return value; always errors because replacement indexing is
unsupported.

No return value; always errors because replacement indexing is
unsupported.

## Details

For \`\[\`: - \`i\` must be character (one or more names).

For \`\[\[\`: - \`i\` must be a single character name.

Numeric/logical indexing and replacement indexing are unsupported for
\`priority_queue\`. Cast with \[as_flexseq()\] for full sequence-style
indexing.

## See also

\[\\.flexseq\], \[as_flexseq()\]

## Examples

``` r
q <- as_priority_queue(c("A", "B"), priorities = c(2, 1), names = c("a", "b"))
q["a"]
#> <priority_queue> size=1
#> min_priority=2 max_priority=2
q[["b"]]
#> [1] "B"
try(q[[1]])
#> Error in `[[.priority_queue`(q, 1) : 
#>   `[[.priority_queue` supports scalar character names only. Cast first with `as_flexseq()`.
```
