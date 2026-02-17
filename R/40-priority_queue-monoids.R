# choose the left-most entry on ties
# Runtime: O(1).
.pq_choose_min <- function(a, b) {
  if(a$priority < b$priority) {
    return(a)
  }
  if(a$priority > b$priority) {
    return(b)
  }
  a
}

# choose the left-most entry on ties
# Runtime: O(1).
.pq_choose_max <- function(a, b) {
  if(a$priority > b$priority) {
    return(a)
  }
  if(a$priority < b$priority) {
    return(b)
  }
  a
}

# Runtime: O(1).
pq_min_measure_monoid <- function() {
  measure_monoid(
    .pq_choose_min,
    list(priority = Inf),
    function(el) list(priority = as.numeric(el$priority))
  )
}

# Runtime: O(1).
pq_max_measure_monoid <- function() {
  measure_monoid(
    .pq_choose_max,
    list(priority = -Inf),
    function(el) list(priority = as.numeric(el$priority))
  )
}

# Runtime: O(1).
.pq_measure_equal <- function(a, b) {
  isTRUE(all.equal(a$priority, b$priority))
}

# Runtime: O(1).
.pq_required_monoids <- function() {
  list(
    .pq_min = pq_min_measure_monoid(),
    .pq_max = pq_max_measure_monoid()
  )
}
