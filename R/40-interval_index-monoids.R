# Runtime: O(1).
.ivx_endpoint_type <- function(v) {
  .ft_scalar_domain(v)
}

# Runtime: O(1).
.ivx_compare_scalar <- function(a, b, endpoint_type = NULL) {
  .ft_scalar_compare(
    a,
    b,
    error_message = "Interval endpoints must support scalar ordering with `<` and `>`."
  )
}

# Runtime: O(1).
.ivx_is_fast_endpoint_type <- function(endpoint_type) {
  .ft_scalar_is_fast_domain(endpoint_type)
}

# Runtime: O(1).
.ivx_compare_scalar_fast <- function(a, b, endpoint_type = NULL) {
  .ft_scalar_compare_fast(
    a,
    b,
    domain = endpoint_type,
    error_message = "Interval endpoints must support scalar ordering with `<` and `>`."
  )
}

# Runtime: O(1).
.ivx_choose_max_start <- function(a, b) {
  if(!isTRUE(a$has)) {
    return(b)
  }
  if(!isTRUE(b$has)) {
    return(a)
  }
  if(!identical(a$endpoint_type, b$endpoint_type)) {
    stop("Incompatible endpoint types encountered in interval_index measures.")
  }
  cmp <- .ivx_compare_scalar(a$start, b$start, a$endpoint_type)
  if(cmp >= 0L) a else b
}

# Runtime: O(1).
.ivx_max_start_monoid <- function() {
  measure_monoid(
    .ivx_choose_max_start,
    list(has = FALSE, start = NULL, endpoint_type = NULL),
    function(el) {
      list(
        has = TRUE,
        start = el$start,
        endpoint_type = .ivx_endpoint_type(el$start)
      )
    }
  )
}

# Runtime: O(m), where m = number of user-supplied monoids.
.ivx_supports_oms_key_type <- function(endpoint_type) {
  isTRUE(endpoint_type %in% c("numeric", "character", "logical"))
}

# Runtime: O(m), where m = number of user-supplied monoids.
.ivx_merge_monoids <- function(monoids = NULL, endpoint_type = NULL) {
  user <- if(is.null(monoids)) list() else monoids
  if(length(user) > 0L) {
    bad <- intersect(names(user), c(".size", ".named_count", ".ivx_max_start", ".oms_max_key"))
    if(length(bad) > 0L) {
      stop(paste0("Reserved monoid names cannot be supplied for interval_index: ", paste(bad, collapse = ", ")))
    }
  }

  req <- list(.ivx_max_start = .ivx_max_start_monoid())
  if(.ivx_supports_oms_key_type(endpoint_type)) {
    req <- c(req, .oms_required_monoids())
  }
  ensure_size_monoids(c(user, req))
}
