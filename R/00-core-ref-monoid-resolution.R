#SO
# note: .size and .count are priveledged and ensured for all trees

# Runtime: O(1) fast-path.
if(FALSE) resolve_tree_monoids <- function(t, required = FALSE) NULL
resolve_tree_monoids(t, required) %::% . : logical : .
resolve_tree_monoids(t, required = FALSE) %as% {
  ms <- attr(t, "monoids", exact = TRUE)
  if(required && is.null(ms)) {
    stop("Tree has no monoids attribute.")
  }
  if(is.null(ms)) {
    return(NULL)
  }
  ms
}

# Runtime: O(1) expected (attribute read + list lookup).
if(FALSE) resolve_named_monoid <- function(t, monoid_name) NULL
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
if(FALSE) merge_monoid_sets <- function(base, add, overwrite = FALSE) NULL
merge_monoid_sets(base, add, overwrite) %::% list : list : logical : list
merge_monoid_sets(base, add, overwrite = FALSE) %as% {
  b <- base
  a <- add

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
  b
}
