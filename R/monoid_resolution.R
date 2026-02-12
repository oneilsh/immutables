resolve_tree_monoids(t, required) %::% . : logical : .
resolve_tree_monoids(t, required = FALSE) %as% {
  ms <- attr(t, "monoids", exact = TRUE)
  if(required && is.null(ms)) {
    stop("Tree has no monoids attribute.")
  }
  if(is.null(ms)) {
    return(NULL)
  }
  ensure_size_monoids(ms)
}

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

emit_concat_assumption_warning(shared_names) %::% character : .
emit_concat_assumption_warning(shared_names) %as% {
  msg <- paste0(
    "concat_trees(): shared monoid names assumed equivalent; left-tree definitions are used for: ",
    paste(shared_names, collapse = ", ")
  )
  w <- structure(
    list(message = msg, call = NULL),
    class = c("fingertree_monoid_assumption_warning", "warning", "condition")
  )
  warning(w)
}
