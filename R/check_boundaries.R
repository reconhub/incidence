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
  res <- try(check_dates(boundary), silent = TRUE)
  if (inherits(res, "try-error")) {
    msg <- paste("%s_date could not be converted to Date. Accepted formats are:",
                 "\n  Date, POSIXct, integer, numeric, character.")
    stop(sprintf(msg, what), call. = FALSE)
  }
  res
}
