# Runtime: O(1) fast-path; optional O(1) validation under fixed monoid set when
# `options(immutables.validate_monoids = TRUE)`.
resolve_tree_monoids(t, required) %::% . : logical : .
resolve_tree_monoids(t, required = FALSE) %as% {
  ms <- attr(t, "monoids", exact = TRUE)
  if(required && is.null(ms)) {
    stop("Tree has no monoids attribute.")
  }
  if(is.null(ms)) {
    return(NULL)
  }
  if(isTRUE(getOption("immutables.validate_monoids", FALSE))) {
    return(ensure_size_monoids(ms))
  }
  ms
}

# Runtime: O(1) expected (attribute read + list lookup).
resolve_named_monoid(t, monoid_name) %::% . : character : list
resolve_named_monoid(t, monoid_name) %as% {
  if(!is.character(monoid_name) || length(monoid_name) != 1L || is.na(monoid_name) || monoid_name == "") {
    stop("`monoid_name` must be a single non-empty string.")
  }
  ms <- resolve_tree_monoids(t, required = TRUE)
  mr <- ms[[monoid_name]]
  if(is.null(mr)) {
    stop(paste0("Monoid name `", monoid_name, "` not found in tree monoids."))
  }
  list(monoids = ms, monoid = mr)
}

# Runtime: O(1) under fixed monoid set.
merge_monoid_sets(base, add, overwrite) %::% list : list : logical : list
merge_monoid_sets(base, add, overwrite = FALSE) %as% {
  b <- ensure_size_monoids(base)
  a <- ensure_size_monoids(add)

  overlap <- intersect(names(b), names(a))
  overlap <- setdiff(overlap, c(".size", ".named_count"))
  if(length(overlap) > 0 && !isTRUE(overwrite)) {
    stop("Monoid names already exist: ", paste(overlap, collapse = ", "), ". Use overwrite = TRUE to replace.")
  }

  if(length(overlap) > 0 && isTRUE(overwrite)) {
    for(nm in overlap) {
      b[[nm]] <- a[[nm]]
    }
  }

  add_only <- setdiff(names(a), names(b))
  if(length(add_only) > 0) {
    b <- c(b, a[add_only])
  }
  ensure_size_monoids(b)
}
