# Runtime: O(1).
.ft_scalar_domain <- function(v) {
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
.ft_scalar_is_fast_domain <- function(domain) {
  isTRUE(domain %in% c("numeric", "character", "logical", "Date", "POSIXct"))
}

# Runtime: O(1).
.ft_scalar_is_missing <- function(v, domain = NULL) {
  if(.ft_scalar_is_fast_domain(domain)) {
    na_flag <- is.na(v)
    return(is.logical(na_flag) && length(na_flag) == 1L && isTRUE(na_flag))
  }
  na_flag <- suppressWarnings(tryCatch(is.na(v), error = function(e) FALSE))
  is.logical(na_flag) && length(na_flag) == 1L && isTRUE(na_flag)
}

# Runtime: O(1).
.ft_scalar_compare <- function(a, b, error_message = "Values must support scalar ordering with `<` and `>`.") {
  lt <- suppressWarnings(tryCatch(a < b, error = function(e) e))
  gt <- suppressWarnings(tryCatch(a > b, error = function(e) e))

  if(
    inherits(lt, "error") ||
    inherits(gt, "error") ||
    !is.logical(lt) ||
    !is.logical(gt) ||
    length(lt) != 1L ||
    length(gt) != 1L ||
    is.na(lt) ||
    is.na(gt)
  ) {
    stop(error_message)
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
.ft_scalar_compare_fast <- function(a, b, domain = NULL, error_message = "Values must support scalar ordering with `<` and `>`.") {
  if(.ft_scalar_is_fast_domain(domain)) {
    lt <- a < b
    gt <- a > b
    if(
      is.logical(lt) &&
      is.logical(gt) &&
      length(lt) == 1L &&
      length(gt) == 1L &&
      !is.na(lt) &&
      !is.na(gt)
    ) {
      if(isTRUE(lt)) {
        return(-1L)
      }
      if(isTRUE(gt)) {
        return(1L)
      }
      return(0L)
    }
  }
  .ft_scalar_compare(a, b, error_message = error_message)
}

# Runtime: O(1).
.ft_scalar_equal_fast <- function(a, b, domain = NULL, error_message = "Values must support scalar ordering with `<` and `>`.") {
  if(.ft_scalar_is_fast_domain(domain)) {
    eq <- a == b
    if(is.logical(eq) && length(eq) == 1L && !is.na(eq)) {
      return(isTRUE(eq))
    }
  }
  .ft_scalar_compare_fast(a, b, domain = domain, error_message = error_message) == 0L
}

# Runtime: O(1).
.ft_scalar_coerce <- function(value, domain) {
  if(identical(domain, "numeric")) {
    return(as.numeric(value))
  }
  if(identical(domain, "character")) {
    return(as.character(value))
  }
  if(identical(domain, "logical")) {
    return(as.logical(value))
  }
  if(identical(domain, "Date")) {
    return(as.Date(value))
  }
  if(identical(domain, "POSIXct")) {
    return(as.POSIXct(value))
  }
  value
}

# Runtime: O(1).
.ft_normalize_scalar_trusted <- function(value, arg_name = "value", value_type = NULL) {
  if(length(value) != 1L) {
    stop(sprintf("`%s` must be a scalar value.", arg_name))
  }
  if(isTRUE(.ft_scalar_is_missing(value, domain = value_type))) {
    stop(sprintf("`%s` must be non-missing.", arg_name))
  }
  resolved_type <- if(is.null(value_type)) .ft_scalar_domain(value) else value_type
  list(value = .ft_scalar_coerce(value, resolved_type), value_type = resolved_type)
}

# Runtime: O(1).
.ft_normalize_scalar_orderable <- function(value, arg_name = "value") {
  norm <- .ft_normalize_scalar_trusted(value, arg_name = arg_name)
  .ft_scalar_compare_fast(
    norm$value,
    norm$value,
    domain = norm$value_type,
    error_message = sprintf("`%s` must support scalar ordering with `<` and `>`.", arg_name)
  )
  norm
}

# Runtime: O(1).
.oms_key_type <- function(key) {
  .ft_scalar_domain(key)
}

# Runtime: O(1).
.oms_is_cpp_key_type <- function(key_type) {
  isTRUE(key_type %in% c("numeric", "character", "logical"))
}

# Runtime: O(1).
.oms_compare_key <- function(a, b, key_type) {
  .ft_scalar_compare_fast(
    a,
    b,
    domain = key_type,
    error_message = "Ordered keys must support scalar ordering with `<` and `>`."
  )
}

# Runtime: O(1).
.oms_normalize_key <- function(key_value) {
  norm <- .ft_normalize_scalar_orderable(key_value, arg_name = "key")
  list(key = norm$value, key_type = norm$value_type)
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
    stop("Incompatible key types encountered in ordered_sequence measures.")
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
      stop(paste0("Reserved monoid names cannot be supplied for ordered_sequence: ", paste(bad, collapse = ", ")))
    }
  }
  ensure_size_monoids(c(user, .oms_required_monoids()))
}
