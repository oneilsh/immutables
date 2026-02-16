# Runtime: O(1).
.oms_key_type <- function(key) {
  if(is.numeric(key)) {
    return("numeric")
  }
  if(is.character(key)) {
    return("character")
  }
  if(is.logical(key)) {
    return("logical")
  }
  NULL
}

# Runtime: O(1).
.oms_compare_key <- function(a, b, key_type) {
  if(key_type == "numeric") {
    if(a < b) {
      return(-1L)
    }
    if(a > b) {
      return(1L)
    }
    return(0L)
  }
  if(key_type == "character") {
    if(a < b) {
      return(-1L)
    }
    if(a > b) {
      return(1L)
    }
    return(0L)
  }
  if(key_type == "logical") {
    ai <- as.integer(isTRUE(a))
    bi <- as.integer(isTRUE(b))
    if(ai < bi) {
      return(-1L)
    }
    if(ai > bi) {
      return(1L)
    }
    return(0L)
  }
  stop("Unsupported key type.")
}

# Runtime: O(1).
.oms_normalize_key <- function(key_value) {
  if(length(key_value) != 1L) {
    stop("`key` must return a scalar value.")
  }
  if(is.na(key_value)) {
    stop("`key` must return a non-missing value.")
  }

  key_type <- .oms_key_type(key_value)
  if(is.null(key_type)) {
    stop("`key` must return a scalar numeric, character, or logical value.")
  }

  if(key_type == "numeric") {
    return(list(key = as.numeric(key_value), key_type = key_type))
  }
  if(key_type == "character") {
    return(list(key = as.character(key_value), key_type = key_type))
  }
  list(key = as.logical(key_value), key_type = key_type)
}

# Runtime: O(1).
.oms_choose_max_key <- function(a, b) {
  if(!isTRUE(a$has)) {
    return(b)
  }
  if(!isTRUE(b$has)) {
    return(a)
  }
  if(!identical(a$key_type, b$key_type)) {
    stop("Incompatible key types encountered in ordered_multiset measures.")
  }
  cmp <- .oms_compare_key(a$key, b$key, a$key_type)
  if(cmp >= 0L) a else b
}

# Runtime: O(1).
oms_max_key_monoid <- function() {
  measure_monoid(
    .oms_choose_max_key,
    list(has = FALSE, key = NULL, key_type = NULL),
    function(el) {
      list(has = TRUE, key = el$key, key_type = .oms_key_type(el$key))
    }
  )
}

# Runtime: O(1).
.oms_required_monoids <- function() {
  list(
    .oms_max_key = oms_max_key_monoid()
  )
}

# Runtime: O(1).
.oms_merge_monoids <- function(monoids = NULL) {
  user <- if(is.null(monoids)) list() else monoids
  if(length(user) > 0L) {
    bad <- intersect(names(user), c(".size", ".named_count", ".oms_max_key"))
    if(length(bad) > 0L) {
      stop(paste0("Reserved monoid names cannot be supplied for ordered_multiset: ", paste(bad, collapse = ", ")))
    }
  }
  ensure_size_monoids(c(user, .oms_required_monoids()))
}
