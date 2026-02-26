# Runtime: O(1).
.ivx_assert_index <- function(x) {
  if(!inherits(x, "interval_index") || !is_structural_node(x)) {
    stop("`x` must be an interval_index.")
  }
  invisible(TRUE)
}

# Runtime: O(1).
.ivx_endpoint_type_state <- function(x) {
  attr(x, "ivx_endpoint_type", exact = TRUE)
}

# Runtime: O(1).
.ivx_normalize_bounds <- function(bounds) {
  if(!is.character(bounds) || length(bounds) != 1L || is.na(bounds)) {
    stop("`bounds` must be one of: [), [], (), (].")
  }
  if(!(bounds %in% c("[)", "[]", "()", "(]"))) {
    stop("`bounds` must be one of: [), [], (), (].")
  }
  bounds
}

# Runtime: O(1).
.ivx_bounds_flags <- function(bounds) {
  list(
    include_start = identical(substr(bounds, 1L, 1L), "["),
    include_end = identical(substr(bounds, 2L, 2L), "]")
  )
}

# Runtime: O(1).
.ivx_normalize_endpoint <- function(value, arg_name, endpoint_type = NULL) {
  if(length(value) != 1L) {
    stop(sprintf("`%s` must be a scalar value.", arg_name))
  }

  na_flag <- suppressWarnings(tryCatch(is.na(value), error = function(e) FALSE))
  if(is.logical(na_flag) && length(na_flag) == 1L && isTRUE(na_flag)) {
    stop(sprintf("`%s` must be non-missing.", arg_name))
  }

  vt <- .ivx_endpoint_type(value)
  if(!is.null(endpoint_type) && !identical(endpoint_type, vt)) {
    stop("Incompatible endpoint type for this interval_index.")
  }

  # Validate orderability for endpoint objects using base Ops.
  .ivx_compare_scalar(value, value, vt)

  list(value = value, endpoint_type = vt)
}

# Runtime: O(1).
.ivx_normalize_interval <- function(start, end, endpoint_type = NULL, start_name = "start", end_name = "end") {
  ns <- .ivx_normalize_endpoint(start, start_name, endpoint_type = endpoint_type)
  ne <- .ivx_normalize_endpoint(end, end_name, endpoint_type = ns$endpoint_type)

  if(.ivx_compare_scalar(ns$value, ne$value, ns$endpoint_type) > 0L) {
    stop("`start` must be <= `end`.")
  }

  list(start = ns$value, end = ne$value, endpoint_type = ns$endpoint_type)
}

.as_interval_index <- function(x, endpoint_type = NULL, bounds = "[)") {
  if(!is_structural_node(x)) {
    stop("Expected a structural tree node.")
  }

  class(x) <- unique(c("interval_index", "ordered_sequence", "flexseq", setdiff(class(x), "list")))
  attr(x, "ivx_endpoint_type") <- endpoint_type
  attr(x, "ivx_bounds") <- .ivx_normalize_bounds(bounds)

  # Keep ordered_sequence key type available only for primitive key classes.
  attr(x, "oms_key_type") <- if(is.null(endpoint_type) || !(endpoint_type %in% c("numeric", "character", "logical"))) {
    NULL
  } else {
    endpoint_type
  }

  x
}

# Runtime: O(1).
.ivx_wrap_like <- function(template, tree, endpoint_type = NULL, bounds = NULL) {
  ep <- if(is.null(endpoint_type)) .ivx_endpoint_type_state(template) else endpoint_type
  b <- .ivx_resolve_bounds(template, bounds)
  .as_interval_index(tree, endpoint_type = ep, bounds = b)
}

# Runtime: O(1).
.ivx_parse_entry <- function(entry, context = "interval_index", endpoint_type = NULL) {
  if(!is.list(entry)) {
    stop(context, " entries must be named lists with fields: item, start, end (optional: key).")
  }

  nm <- names(entry)
  if(is.null(nm) || any(is.na(nm)) || any(nm == "")) {
    stop(context, " entries must be named lists with fields: item, start, end (optional: key).")
  }
  if(anyDuplicated(nm) > 0L) {
    stop(context, " entry fields must be unique.")
  }

  bad <- setdiff(nm, c("item", "start", "end", "key"))
  if(length(bad) > 0L) {
    stop(context, " entry contains unsupported field(s): ", paste(bad, collapse = ", "))
  }
  if(!("item" %in% nm) || !("start" %in% nm) || !("end" %in% nm)) {
    stop(context, " entries must include `item`, `start`, and `end`.")
  }

  norm <- .ivx_normalize_interval(entry[["start"]], entry[["end"]], endpoint_type = endpoint_type)

  if("key" %in% nm) {
    if(.ivx_compare_scalar(entry[["key"]], norm$start, norm$endpoint_type) != 0L) {
      stop(context, " entry `key` must equal `start`.")
    }
  }

  list(
    entry = .ivx_make_entry(entry[["item"]], norm$start, norm$end),
    endpoint_type = norm$endpoint_type
  )
}

# Runtime: O(n) to validate all entries.
.ivx_validate_tree_entries <- function(x, endpoint_type = NULL, context = "interval_index") {
  els <- .ft_to_list(x)
  if(length(els) == 0L) {
    return(endpoint_type)
  }

  out_type <- endpoint_type
  for(el in els) {
    parsed <- .ivx_parse_entry(el, context = context, endpoint_type = out_type)
    out_type <- parsed$endpoint_type
  }

  out_type
}

# Runtime: O(n) validation + O(1) wrap.
.ivx_restore_tree <- function(x, template = NULL, context = "interval_index") {
  endpoint_type <- if(is.null(template)) NULL else .ivx_endpoint_type_state(template)
  bounds <- if(is.null(template)) "[)" else .ivx_resolve_bounds(template, NULL)
  endpoint_type <- .ivx_validate_tree_entries(x, endpoint_type = endpoint_type, context = context)
  .as_interval_index(x, endpoint_type = endpoint_type, bounds = bounds)
}

.ivx_is_structural_fast <- function(node) {
  cls <- class(node)
  !is.null(cls) && any(cls %in% c("Empty", "Single", "Deep", "Digit", "Node"))
}

# Runtime: O(n).
.ivx_entries <- function(x) {
  ms <- attr(x, "measures", exact = TRUE)
  n <- if(!is.null(ms) && !is.null(ms[[".size"]])) {
    as.integer(ms[[".size"]])
  } else {
    as.integer(node_measure(x, ".size"))
  }

  if(is.na(n) || n <= 0L) {
    return(list())
  }

  st <- new.env(parent = emptyenv())
  st$out <- vector("list", n)
  st$pos <- 1L

  fill <- function(node) {
    if(!.ivx_is_structural_fast(node)) {
      st$out[[st$pos]] <- node
      st$pos <- st$pos + 1L
      return(invisible(NULL))
    }

    if(inherits(node, "Empty")) {
      return(invisible(NULL))
    }
    if(inherits(node, "Single")) {
      fill(.subset2(node, 1L))
      return(invisible(NULL))
    }
    if(inherits(node, "Deep")) {
      fill(.subset2(node, "prefix"))
      fill(.subset2(node, "middle"))
      fill(.subset2(node, "suffix"))
      return(invisible(NULL))
    }

    for(el in node) {
      fill(el)
    }
    invisible(NULL)
  }

  fill(x)
  if(!identical(st$pos, as.integer(n + 1L))) {
    return(.ft_to_list(x))
  }
  st$out
}

# Runtime: O(m), where m = number of attached monoids.
.ivx_user_monoids <- function(x) {
  ms <- attr(x, "monoids", exact = TRUE)
  out <- ms[setdiff(names(ms), c(".size", ".named_count", ".ivx_max_start", ".ivx_max_end", ".ivx_min_end", ".oms_max_key"))]
  if(length(out) == 0L) {
    return(NULL)
  }
  out
}

# Runtime: O(1).
.ivx_empty_like <- function(template) {
  ms <- resolve_tree_monoids(template, required = TRUE)
  .ivx_wrap_like(template, empty_tree(monoids = ms))
}

# Runtime: O(1).
.ivx_resolve_bounds <- function(x, bounds = NULL) {
  if(!is.null(bounds)) {
    return(.ivx_normalize_bounds(bounds))
  }
  b <- attr(x, "ivx_bounds", exact = TRUE)
  if(is.null(b)) "[)" else b
}
