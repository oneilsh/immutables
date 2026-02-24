# Add/merge monoids on an existing tree

Add/merge monoids on an existing tree

## Usage

``` r
add_monoids(t, monoids, overwrite = FALSE)
```

## Arguments

- t:

  FingerTree.

- monoids:

  Named list of \`measure_monoid\` objects to add.

- overwrite:

  Logical; whether overlapping names replace existing monoids.

## Value

A persistent copy with recomputed cached measures.

## Details

\`add_monoids()\` is the advanced user-facing API for attaching custom
monoids after constructing a structure.

The \`measure(el)\` input shape depends on structure type:

\- \`flexseq\`: payload element. - \`priority_queue\`: entry list with
\`item\` and \`priority\`. - \`ordered_sequence\`: entry list with
\`item\` and \`key\`. - \`interval_index\`: entry list with \`item\`,
\`start\`, and \`end\`.

## Examples

``` r
# Common workflow:
# 1) construct structure
# 2) attach monoid with add_monoids()
# 3) query with split_around_by_predicate()

# flexseq: split a stream by cumulative token budget
tokens <- as_flexseq(c(120, 80, 50, 200))
token_sum <- measure_monoid(`+`, 0, as.numeric)
budgeted <- add_monoids(tokens, list(token_sum = token_sum))
cut <- split_around_by_predicate(budgeted, function(v) v > 200, "token_sum")
cut$left   # still within budget
#> Unnamed flexseq with 2 elements.
#> Custom monoids: token_sum
#> 
#> Elements:
#> 
#> [[1]]
#> [1] 120
#> 
#> [[2]]
#> [1] 80
#> 
cut$elem   # first element that crosses budget
#> [1] 50
cut$right  # remaining tail
#> Unnamed flexseq with 1 element.
#> Custom monoids: token_sum
#> 
#> Elements:
#> 
#> [[1]]
#> [1] 200
#> 

# interval_index: split by cumulative interval width
ix <- interval_index("A", "B", "C", start = c(0, 4, 9), end = c(3, 8, 10))
width_sum <- measure_monoid(`+`, 0, function(el) as.numeric(el$end - el$start))
ix2 <- add_monoids(ix, list(width_sum = width_sum))
cut_ix <- split_around_by_predicate(as_flexseq(ix2), function(v) v > 6, "width_sum")
cut_ix$left        # intervals before width budget is exceeded
#> Unnamed flexseq with 1 element.
#> Custom monoids: width_sum
#> 
#> Elements:
#> 
#> [[1]]
#> $item
#> [1] "A"
#> 
#> $start
#> [1] 0
#> 
#> $end
#> [1] 3
#> 
#> $key
#> [1] 0
#> 
#> 
cut_ix$elem        # first interval that crosses width budget (value at cut_ix$elem$item)
#> $item
#> [1] "B"
#> 
#> $start
#> [1] 4
#> 
#> $end
#> [1] 8
#> 
#> $key
#> [1] 4
#> 
cut_ix$right       # remaining interval
#> Unnamed flexseq with 1 element.
#> Custom monoids: width_sum
#> 
#> Elements:
#> 
#> [[1]]
#> $item
#> [1] "C"
#> 
#> $start
#> [1] 9
#> 
#> $end
#> [1] 10
#> 
#> $key
#> [1] 9
#> 
#> 
```
