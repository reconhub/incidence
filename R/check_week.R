#' Check for a valid week interval
#'
#' @param the_interval character, integer, or numeric
#'
#' @return a logical value indicating if any of the tests pass
#' @noRd
#' @keywords internal
check_week <- function(the_interval) {
  num_week  <- is.numeric(the_interval) && the_interval == 7
  int_week  <- is.integer(the_interval) && the_interval == 7L
  char_week <- is.character(the_interval) && grepl("week", the_interval, ignore.case = TRUE)
  num_week || int_week || char_week
}
