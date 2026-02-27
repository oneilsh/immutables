# Split by Scalar Index or Name

Splits by element position (\`.size\` measure) after resolving \`at\` to
a single index. \`at\` can be a positive scalar integer index or a
scalar character name.

## Usage

``` r
split_at(x, at, pull_index = FALSE)
```

## Arguments

- x:

  A \`flexseq\`.

- at:

  Positive scalar integer index or scalar character name.

- pull_index:

  Logical switch between two-way and three-way split shape. If \`TRUE\`,
  uses \[split_by_predicate()\] and returns \`list(left, right)\`. If
  \`FALSE\`, uses \[split_around_by_predicate()\] and returns
  \`list(left, elem, right)\`.

## Value

A split list, shape controlled by \`pull_index\`.
