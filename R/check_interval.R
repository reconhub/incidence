#' Check the interval between bins
#'
#' This enforces that an interval is:
#' - strictly positive
#' - integer (rounded) OR compatibile with date
#' - finite
#' - of length 1
#'
#' @param x an integer or numeric interval
#' @return an integer interval
#' @noRd
check_interval <- function(x){
  if (missing(x) || is.null(x)) {
    stop("Interval is missing or NULL")
  }
  if (length(x) != 1L) {
    stop(sprintf(
      "Exactly one value should be provided as interval (%d provided)",
      length(x)))
  }
  if (!is.finite(x)) {
    if (is.character(x)) {
      x <- valid_interval_character(x)
    } else {
      stop("Interval is not finite")
    }
  }
  if (is.numeric(x)) {
    x <- as.integer(round(old <- x))
  }
  if (x < 1L) {
    stop(sprintf(
      "Interval must be at least 1 (input: %.3f; after rounding: %d)",
      old, x))
  }
  x
}
