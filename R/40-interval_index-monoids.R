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
.ivx_merge_monoids <- function(monoids = NULL) {
  user <- if(is.null(monoids)) list() else monoids
  if(length(user) > 0L) {
    bad <- intersect(names(user), c(".size", ".named_count"))
    if(length(bad) > 0L) {
      stop(paste0("Reserved monoid names cannot be supplied for interval_index: ", paste(bad, collapse = ", ")))
    }
  }
  base <- if(length(user) == 0L) list(.size = size_measure_monoid()) else user
  ensure_size_monoids(base)
}
