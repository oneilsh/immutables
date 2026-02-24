# Printing guidelines

Note: there may be typos or errors, please infer the intent from context.

# Flexseq

Basically a list view with a little metadata at the top, but showing the first and the last with a ... (skipping) for the middle part if any. We only show the names of the user-defined measures, since those are complex and likely to be a rare use case anyway, and that line only if there are any custom measurements.

If it's not named, it should say "Unnamed flexseq with 91 elements." and use [[1]] etc.

```
> flexseq(one = c(10, 20), 
             two = c(4, 5), 
             three = c(8, 6), 
             #...
             ninety = c(5, 2),
             ninetyone = c(8, 10)) |> add_monoids(list(product = ..., summation = ...))
Named flexseq with 91 elements.
Custom monoids: product, summation

Elements:

$one
[1] 10 20

$two
[1] 4 5

... (skipping 87 elements)

$ninety
[1] 5 2

$ninetyone
[1] 8 10
```

# Priority Queue

Hmm, so the way additional monoids work is that the *monoid* gets the data elements, but the *measure function* is getting the full thing. 

```
priority_queue(one = c(10, 20), 
              two = c(4, 5), 
              three = c(8, 6), 
              #...
              ninety = c(5, 2),
              ninetyone = c(8, 10), 
              priorities = c(4, 4.5, 2.1, 7, 6))
Named priority_queue with 91 elements.
Minimum priority: 2.1, Maximum priority: 7

Elements (by priority):

$three (priority 2.1)
[1] 8 6

$one (priority 4)
[1] 10 20

... (skipping 87 elements)

$ninetyone (priority 6)
[1] 8 10

$ninety (priority 7)
[1] 5 2
```

This one is trickier for unnamed priority queues because we don't have [[1]] etc to fall back on (we disabled position-based indexing). For those I think we can just show them like:

```
Unnamed priority_queue with 91 elements.
Minimum priority: 2.1, Maximum priority: 7

(priority 2.1)
[1] 8 6

(priority 4)
[1] 10 20

... (skipping 87 elements)

(priority 6)
[1] 8 10

(priority 7)
[1] 5 2
```

# Ordered sequence

Ordered sequences work like priority queues:

```
ordered_sequence(one = c(10, 20), 
              two = c(4, 5), 
              three = c(8, 6), 
              #...
              ninety = c(5, 2),
              ninetyone = c(8, 10), 
              keys = c(1, 2, 3, 90, 91))
Named ordered_sequence with 91 elements.

Elements (by key order):

$one (key 1)
[1] 8 6

$two (key 2)
[1] 10 20

... (skipping 87 elements)

$ninety (key 90)
[1] 8 10

$ninety (key 91)
[1] 5 2
```

Since we support indexing with `[[` we can use this nomenclature for unnamed sequences:

ordered_sequence(c(10, 20), 
              c(4, 5), 
              tc(8, 6), 
              #...
              c(5, 2),
              nc(8, 10), 
              keys = c(1, 2, 3, 90, 91))
Named ordered_sequence with 91 elements.

Elements (by key order):

[[1]] (key 1)
[1] 8 6

[[2]] (key 2)
[1] 10 20

... (skipping 87 elements)

[[90]] (key 90)
[1] 8 10

[[91]] (key 91)
[1] 5 2
```

# Interval indices

Interval indices work like ordered sequences, including `[[`:

interval_index(one = c(10, 20), 
              two = c(4, 5), 
              three = c(8, 6), 
              #...
              ninety = c(5, 2),
              ninetyone = c(8, 10), 
              start = c(2, 4, 6, 8, 10),
              end = c(12, 14, 16, 18, 20))
Named interval_index with 91 elements, default bounds [start, end)

Elements (by interval start order):

$one (interval [2, 12))
[1] 8 6

$two (interval [4, 14))
[1] 10 20

... (skipping 87 elements)

$ninety (interval [8, 18))
[1] 8 10

$ninety (interval [18, 20))
[1] 5 2
```

Unnamed:

```
Unnamed interval_index with 91 elements, default bounds [start, end)

Elements (by interval start order):

[[1]] (interval [2, 12))
[1] 8 6

[[2]] (interval [4, 14))
[1] 10 20

... (skipping 87 elements)

[[90]] (interval [8, 18))
[1] 8 10

[[91]] (interval [18, 20))
[1] 5 2
```