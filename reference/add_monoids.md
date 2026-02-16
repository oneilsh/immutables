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

## Examples

``` r
x <- as_flexseq(1:5)
x
#> FingerTree <size=5, named=no>
#>   monoids: none
#> 
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 5
#> 

sum_m <- measure_monoid(`+`, 0, as.numeric)
x2 <- add_monoids(x, list(sum = sum_m))
x2
#> $prefix
#> [[1]]
#> [1] 1
#> 
#> attr(,"class")
#> [1] "Digit" "list" 
#> attr(,"monoids")
#> attr(,"monoids")$.size
#> $f
#> function(a, b) a + b
#> <environment: 0x55f7a07884d0>
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function(el) 1
#> <environment: 0x55f7a07884d0>
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"monoids")$.named_count
#> $f
#> function(a, b) a + b
#> <environment: 0x55f7a04054c8>
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function(el) {
#>     if (isTRUE(.ft_has_name(el)))
#>       1L
#>     else 0L
#>   }
#> <environment: 0x55f7a04054c8>
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"monoids")$sum
#> $f
#> function (e1, e2)  .Primitive("+")
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function (x, ...)  .Primitive("as.double")
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"measures")
#> attr(,"measures")$.size
#> [1] 1
#> 
#> attr(,"measures")$.named_count
#> [1] 0
#> 
#> attr(,"measures")$sum
#> [1] 1
#> 
#> 
#> $middle
#> [[1]]
#> NULL
#> 
#> attr(,"class")
#> [1] "Empty"      "FingerTree" "list"      
#> attr(,"monoids")
#> attr(,"monoids")$.size
#> $f
#> function(a, b) a + b
#> <environment: 0x55f7a0f57598>
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function(el) 1
#> <environment: 0x55f7a0f57598>
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"monoids")$.named_count
#> $f
#> function(a, b) a + b
#> <environment: 0x55f7a0ed3ca8>
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function(el) {
#>     if (isTRUE(.ft_has_name(el)))
#>       1L
#>     else 0L
#>   }
#> <environment: 0x55f7a0ed3ca8>
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"monoids")$sum
#> $f
#> function (e1, e2)  .Primitive("+")
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function (x, ...)  .Primitive("as.double")
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"measures")
#> attr(,"measures")$.size
#> [1] 0
#> 
#> attr(,"measures")$.named_count
#> [1] 0
#> 
#> attr(,"measures")$sum
#> [1] 0
#> 
#> 
#> $suffix
#> [[1]]
#> [1] 2
#> 
#> [[2]]
#> [1] 3
#> 
#> [[3]]
#> [1] 4
#> 
#> [[4]]
#> [1] 5
#> 
#> attr(,"class")
#> [1] "Digit" "list" 
#> attr(,"monoids")
#> attr(,"monoids")$.size
#> $f
#> function(a, b) a + b
#> <environment: 0x55f7a2a1a360>
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function(el) 1
#> <environment: 0x55f7a2a1a360>
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"monoids")$.named_count
#> $f
#> function(a, b) a + b
#> <environment: 0x55f7a2a35250>
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function(el) {
#>     if (isTRUE(.ft_has_name(el)))
#>       1L
#>     else 0L
#>   }
#> <environment: 0x55f7a2a35250>
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"monoids")$sum
#> $f
#> function (e1, e2)  .Primitive("+")
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function (x, ...)  .Primitive("as.double")
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"measures")
#> attr(,"measures")$.size
#> [1] 4
#> 
#> attr(,"measures")$.named_count
#> [1] 0
#> 
#> attr(,"measures")$sum
#> [1] 14
#> 
#> 
#> attr(,"class")
#> [1] "Deep"       "FingerTree" "list"      
#> attr(,"monoids")
#> attr(,"monoids")$.size
#> $f
#> function(a, b) a + b
#> <environment: 0x55f7a3370678>
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function(el) 1
#> <environment: 0x55f7a3370678>
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"monoids")$.named_count
#> $f
#> function(a, b) a + b
#> <environment: 0x55f7a3399b58>
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function(el) {
#>     if (isTRUE(.ft_has_name(el)))
#>       1L
#>     else 0L
#>   }
#> <environment: 0x55f7a3399b58>
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"monoids")$sum
#> $f
#> function (e1, e2)  .Primitive("+")
#> 
#> $i
#> [1] 0
#> 
#> $measure
#> function (x, ...)  .Primitive("as.double")
#> 
#> attr(,"class")
#> [1] "measure_monoid" "MeasureMonoid"  "list"          
#> 
#> attr(,"measures")
#> attr(,"measures")$.size
#> [1] 5
#> 
#> attr(,"measures")$.named_count
#> [1] 0
#> 
#> attr(,"measures")$sum
#> [1] 15
#> 
attr(x2, "measures")$sum
#> [1] 15

# replace an existing monoid definition
prod_m <- measure_monoid(`*`, 1, as.numeric)
x3 <- add_monoids(x2, list(sum = prod_m), overwrite = TRUE)
attr(x3, "measures")$sum
#> [1] 120
```
