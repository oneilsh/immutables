# Runtime: O(1).
.pq_priority_type_state <- function(x) {
  attr(x, "pq_priority_type", exact = TRUE)
}

# Runtime: O(1).
.pq_validate_priority_type <- function(pq_priority_type, new_priority_type) {
  if(is.null(pq_priority_type)) {
    return(new_priority_type)
  }
  if(!identical(pq_priority_type, new_priority_type)) {
    stop("Incompatible priority type for this priority_queue.")
  }
  pq_priority_type
}

# Runtime: O(1).
.pq_normalize_priority <- function(priority) {
  norm <- .ft_normalize_scalar_orderable(priority, arg_name = "priority")
  list(priority = norm$value, priority_type = norm$value_type)
}

# Runtime: O(1).
.pq_assert_queue <- function(q) {
  if(!inherits(q, "priority_queue") || !is_structural_node(q)) {
    stop("`q` must be a priority_queue.")
  }
  invisible(TRUE)
}

# Runtime: O(n) over tree size for any non-trivial update (rebind/recompute pass).
#' @method add_monoids priority_queue
#' @export
#' @noRd
add_monoids.priority_queue <- function(t, monoids, overwrite = FALSE) {
  if(length(monoids) > 0L) {
    bad <- intersect(names(monoids), c(".size", ".named_count", ".pq_min", ".pq_max"))
    if(length(bad) > 0L) {
      stop("Reserved monoid names cannot be supplied for priority_queue: ", paste(bad, collapse = ", "))
    }
  }
  add_monoids.flexseq(t, monoids, overwrite = overwrite)
}

.pq_parse_entry <- function(entry, context = "priority_queue", priority_type = NULL) {
  if(!is.list(entry)) {
    stop(context, " entries must be named lists with fields: item, priority (optional: name).")
  }
  nm <- names(entry)
  if(is.null(nm) || any(is.na(nm)) || any(nm == "")) {
    stop(context, " entries must be named lists with fields: item, priority (optional: name).")
  }
  if(anyDuplicated(nm) > 0L) {
    stop(context, " entry fields must be unique.")
  }
  bad <- setdiff(nm, c("item", "priority", "name", "seq_id"))
  if(length(bad) > 0L) {
    stop(context, " entry contains unsupported field(s): ", paste(bad, collapse = ", "))
  }
  if(!("item" %in% nm) || !("priority" %in% nm)) {
    stop(context, " entries must include both `item` and `priority`.")
  }

  norm <- .pq_normalize_priority(entry[["priority"]])
  resolved_type <- .pq_validate_priority_type(priority_type, norm$priority_type)
  out <- list(item = entry[["item"]], priority = norm$priority)

  nm_hint <- if("name" %in% nm) .ft_normalize_name(entry[["name"]]) else NULL
  if(is.null(nm_hint)) {
    nm_hint <- .ft_get_name(entry)
  }
  list(entry = .ft_set_name(out, nm_hint), priority_type = resolved_type)
}

# Runtime: O(n) to validate all entries.
.pq_validate_tree_entries <- function(x, context = "priority_queue", priority_type = NULL) {
  els <- .ft_to_list(x)
  if(length(els) == 0L) {
    return(priority_type)
  }
  out_type <- priority_type
  for(el in els) {
    parsed <- .pq_parse_entry(el, context = context, priority_type = out_type)
    out_type <- parsed$priority_type
  }
  out_type
}

# Runtime: O(n) to validate then O(1) to reclass.
.pq_restore_tree <- function(x, template = NULL, context = "priority_queue") {
  priority_type <- if(is.null(template)) NULL else .pq_priority_type_state(template)
  priority_type <- .pq_validate_tree_entries(x, context = context, priority_type = priority_type)
  .as_priority_queue(x, priority_type = priority_type)
}

# Runtime: O(1).
.pq_make_entry <- function(item, priority, priority_type = NULL) {
  norm <- .pq_normalize_priority(priority)
  list(
    entry = list(item = item, priority = norm$priority),
    priority_type = .pq_validate_priority_type(priority_type, norm$priority_type)
  )
}

# Runtime: O(1) under fixed monoid set.
.pq_merge_monoids <- function(monoids = NULL) {
  user <- if(is.null(monoids)) list() else monoids
  if(length(user) > 0L) {
    bad <- intersect(names(user), c(".size", ".named_count", ".pq_min", ".pq_max"))
    if(length(bad) > 0L) {
      stop(paste0("Reserved monoid names cannot be supplied for priority queues: ", paste(bad, collapse = ", ")))
    }
  }
  c(user, .pq_required_monoids())
}

# Runtime: O(1).
.as_priority_queue <- function(x, priority_type = NULL) {
  if(!is_structural_node(x)) {
    stop("Expected a structural tree node.")
  }
  class(x) <- unique(c("priority_queue", "flexseq", setdiff(class(x), "list")))
  attr(x, "pq_next_seq") <- NULL
  attr(x, "pq_priority_type") <- priority_type
  x
}

# Runtime: O(1).
.pq_wrap_like <- function(template, tree, priority_type = NULL) {
  resolved_type <- if(is.null(priority_type)) .pq_priority_type_state(template) else priority_type
  .as_priority_queue(tree, priority_type = resolved_type)
}
