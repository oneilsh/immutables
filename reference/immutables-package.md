# immutables

Sequence objects (\`flexseq()\`) supporting indexed and named access,
appending and prepending, concatenation, splitting, fast push and pop
from either end, item removal, and more.

## Details

Also implemented are priority queues (\`priority_queue()\`) focused on
queue-oriented operations, including min and max peeking and popping by
priority value.

Backed by monoid-annotated 2-3 fingertrees, all structures are
persistent (operations return effective modified copies), and most
operations are constant time, amortized constant time, or \\O(\log n)\\
(indexing k elements is \\O(k \log n)\\). The developer API supports the
addition of custom structures via combinations of monoids and measures;
see vignettes for details.

## See also

Useful links:

- <https://oneilsh.github.io/immutables>

- <https://github.com/oneilsh/immutables>

- Report bugs at <https://github.com/oneilsh/immutables/issues>

## Author

**Maintainer**: Shawn T. O'Neil <shawn@tislab.org>
