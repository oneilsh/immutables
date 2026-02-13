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
.pq_assert_entry <- function(el) {
  if(!is.list(el) || !all(c("item", "priority", "seq_id") %in% names(el))) {
    stop("Priority queue elements must be encoded entries with item/priority/seq_id.")
  }
  if(!is.numeric(el$priority) || length(el$priority) != 1L || is.na(el$priority)) {
    stop("Priority queue entry priority must be a single non-missing numeric value.")
  }
  if(!is.numeric(el$seq_id) || length(el$seq_id) != 1L || is.na(el$seq_id)) {
    stop("Priority queue entry seq_id must be a single non-missing numeric value.")
  }
  invisible(TRUE)
}

# Runtime: O(1).
.pq_measure_from_entry <- function(el) {
  .pq_assert_entry(el)
  list(priority = as.numeric(el$priority), seq_id = as.numeric(el$seq_id))
}

# Runtime: O(1).
pq_min_measure_monoid <- function() {
  measure_monoid(
    .pq_choose_min,
    list(priority = Inf, seq_id = Inf),
    .pq_measure_from_entry
  )
}

# Runtime: O(1).
pq_max_measure_monoid <- function() {
  measure_monoid(
    .pq_choose_max,
    list(priority = -Inf, seq_id = Inf),
    .pq_measure_from_entry
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
