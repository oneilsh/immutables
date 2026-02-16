#' Walk a Function Over Sequence Elements
#'
#' Applies `f` to each element in left-to-right order for side effects.
#'
#' @param x A `flexseq`.
#' @param f Function applied to each element.
#' @param ... Additional arguments passed to `f`.
#' @return Invisibly returns `x`.
#' @examples
#' x <- as_flexseq(1:4)
#' seq_walk(x, function(v) cat(v, "\\n"))
#' @export
# Runtime: O(n) over number of elements.
seq_walk <- function(x, f, ...) {
  if(!inherits(x, "flexseq")) {
    stop("`x` must be a flexseq.")
  }
  if(!is.function(f)) {
    stop("`f` must be a function.")
  }

  els <- .ft_to_list(x)
  for(el in els) {
    f(.ft_strip_name(el), ...)
  }
  invisible(x)
}
