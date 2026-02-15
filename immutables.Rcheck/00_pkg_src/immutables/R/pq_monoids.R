# choose the earliest insertion on ties
# Runtime: O(1).
.pq_choose_min <- function(a, b) {
  if(a$priority < b$priority) {
    return(a)
  }
  if(a$priority > b$priority) {
    return(b)
  }
  if(a$seq_id <= b$seq_id) a else b
}

# choose the earliest insertion on ties
# Runtime: O(1).
.pq_choose_max <- function(a, b) {
  if(a$priority > b$priority) {
    return(a)
  }
  if(a$priority < b$priority) {
    return(b)
  }
  if(a$seq_id <= b$seq_id) a else b
}

# Runtime: O(1).
pq_min_measure_monoid <- function() {
  measure_monoid(
    .pq_choose_min,
    list(priority = Inf, seq_id = Inf),
    function(el) list(priority = as.numeric(el$priority), seq_id = as.numeric(el$seq_id))
  )
}

# Runtime: O(1).
pq_max_measure_monoid <- function() {
  measure_monoid(
    .pq_choose_max,
    list(priority = -Inf, seq_id = Inf),
    function(el) list(priority = as.numeric(el$priority), seq_id = as.numeric(el$seq_id))
  )
}

# Runtime: O(1).
.pq_measure_equal <- function(a, b) {
  isTRUE(all.equal(a$priority, b$priority)) && isTRUE(all.equal(a$seq_id, b$seq_id))
}

# Runtime: O(1).
.pq_required_monoids <- function() {
  list(
    .pq_min = pq_min_measure_monoid(),
    .pq_max = pq_max_measure_monoid()
  )
}
