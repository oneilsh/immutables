# Build graph data frames for a finger tree

Build graph data frames for a finger tree

## Usage

``` r
get_graph_df(t)
```

## Arguments

- t:

  FingerTree.

## Value

A list with edge and node data frames for igraph plotting.

## Examples

``` r
if (FALSE) { # \dontrun{
t <- as_flexseq(letters[1:4])
gdf <- get_graph_df(t)
names(gdf)
} # }
```
