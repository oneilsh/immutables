# Runtime: O(1).
.ivx_endpoint_type <- function(v) {
  if(inherits(v, "Date")) {
    return("Date")
  }
  if(inherits(v, "POSIXct")) {
    return("POSIXct")
  }
  if(is.numeric(v)) {
    return("numeric")
  }
  if(is.character(v)) {
    return("character")
  }
  if(is.logical(v)) {
    return("logical")
  }
  cls <- class(v)
  if(length(cls) > 0L) {
    return(paste0("class:", paste(cls, collapse = "/")))
  }
  paste0("typeof:", typeof(v))
}

# Runtime: O(1).
.ivx_compare_scalar <- function(a, b, endpoint_type = NULL) {
  lt <- suppressWarnings(tryCatch(a < b, error = function(e) e))
  gt <- suppressWarnings(tryCatch(a > b, error = function(e) e))

  if(inherits(lt, "error") || inherits(gt, "error") || length(lt) != 1L || length(gt) != 1L) {
    stop("Interval endpoints must support scalar ordering with `<` and `>`.")
  }
  if(is.na(lt) || is.na(gt)) {
    stop("Interval endpoints must support scalar ordering with `<` and `>`.")
  }

  if(isTRUE(lt)) {
    return(-1L)
  }
  if(isTRUE(gt)) {
    return(1L)
  }
  0L
}

# Runtime: O(1).
.ivx_is_fast_endpoint_type <- function(endpoint_type) {
  isTRUE(endpoint_type %in% c("numeric", "character", "logical", "Date", "POSIXct"))
}

# Runtime: O(1).
.ivx_compare_scalar_fast <- function(a, b, endpoint_type = NULL) {
  if(.ivx_is_fast_endpoint_type(endpoint_type)) {
    lt <- a < b
    gt <- a > b
    if(length(lt) == 1L && length(gt) == 1L && !is.na(lt) && !is.na(gt)) {
      if(isTRUE(lt)) {
        return(-1L)
      }
      if(isTRUE(gt)) {
        return(1L)
      }
      return(0L)
    }
  }
  .ivx_compare_scalar(a, b, endpoint_type = endpoint_type)
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
.ivx_merge_monoids <- function(monoids = NULL) {
  user <- if(is.null(monoids)) list() else monoids
  if(length(user) > 0L) {
    bad <- intersect(names(user), c(".size", ".named_count", ".ivx_max_start"))
    if(length(bad) > 0L) {
      stop(paste0("Reserved monoid names cannot be supplied for interval_index: ", paste(bad, collapse = ", ")))
    }
  }
  ensure_size_monoids(c(user, list(.ivx_max_start = .ivx_max_start_monoid())))
}
