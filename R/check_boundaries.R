#' A cromulence check for first_date and _last date
#'
#' This will create a boundary from the data if there is none provided.
#'
#' @param dates a vector of dates, integers, or numerics
#' @param boundary a date, integer, numeric, or character string that can resolve to a date
#' @param what "first" or "last" for the first_date and last_date arguments
#'
#' @return a date or integer
#' @noRd
#' @keywords internal
check_boundaries <- function(dates, boundary = NULL, what = "first") {
  if (is.null(boundary)) {
    MINMAX <- if (what == "first") min else max
    boundary <- MINMAX(dates, na.rm = TRUE)
  }
  msg <- "%s_date (%s) could not be converted to Date."
  if (is.character(boundary) && !grepl("^[0-9]{4}-[01][0-9]-[0-3][0-9]$", boundary)) {
    msg <- paste(msg, 'Dates must be in ISO 8601 standard format (yyyy-mm-dd).')
    stop(sprintf(msg, what, boundary), call. = FALSE)
  }
  res <- try(check_dates(boundary), silent = TRUE)
  if (inherits(res, "try-error")) {
    msg <- paste(msg, "Accepted formats are:",
                 "\n  Date, POSIXct, integer, numeric, character.")
    stop(sprintf(msg, what, deparse(substitute(boundary))), call. = FALSE)
  }
  res
}
