tree_chars <- function(t) {
  vals <- as.list(t)
  if(length(vals) == 0L) {
    return("")
  }
  paste0(vapply(vals, as.character, character(1)), collapse = "")
}
