#' Make breaks with dates
#'
#' Because Date objects have a specific `seq` method, it's possible
#' to make breaks with both integers and date objects. This function
#' will check to make sure that the interval is valid.
#'
#' @param date an integer, numeric, or Date vector
#' @param the_interval an integer or character
#' @param last_date an integer, numeric, or Date
#' @param first_date an integer, numeric, or Date
#' @param dots a named list of options
#' @param null_first_date a logical specifying whether or not the first_date
#'   argument was NULL in the original call.
#'
#' @author Zhian Kamvar
#' @return a vector of integers or Dates
#' @noRd
#' @examples
#'
#' set.seed(999)
#' d <- sample(10, replace = TRUE)
#' make_breaks_easier(d, 2L)
make_breaks_easier <- function(dates, the_interval, first_date = NULL,
                               last_date = NULL, dots = 1L,
                               null_first_date = TRUE) {

  the_interval    <- valid_interval_character(the_interval)
  date_interval   <- is.character(the_interval) && is_date_interval(the_interval)
  uneven_interval <- date_interval && the_interval %in% c("month", "quarter", "year")

  # getting information about the date
  fd        <- as.character(first_date)
  the_day   <- as.integer(substr(fd, 9, 10))
  the_month <- as.integer(substr(fd, 6, 7))

  if ("standard" %in% names(dots)) {
    if (isTRUE(dots$standard) && null_first_date) {
      is_a_week <- !uneven_interval && check_week(the_interval)
      if (is_a_week) {
        # This returns something like 2018-W29
        first_isoweek <- aweek::date2week(first_date, 1L, floor_day = TRUE)
        # here we convert it back to a date
        first_date    <- aweek::week2date(first_isoweek)
      }
      if (uneven_interval) {
        # Replace the day with the first day of the month
        substr(fd, 9, 10) <- "01"
        if (the_interval == "quarter") {
          # Replace the month with the first month of the quarter
          m <- (as.integer(substr(fd, 6, 7)) - 1L) %/% 3L
          substr(fd, 6, 7) <- sprintf("%02d", (m * 3) + 1L)
        }
        if (the_interval == "year") {
          # Replace the month with the first month of the year
          substr(fd, 6, 7) <- "01"
        }
        # re-cast the date
        first_date <- as.Date(fd)
      }
    } else {
      if (uneven_interval && the_interval != "year" && the_day > 28) {
        # The first date represents a day that doesn't occur in all months
        msg <- paste("The first_date (%s) represents a day that does not",
                     "occur in all months. Because of this, bins may not",
                     "conform to monthly boundaries. To prevent this",
                     "behavior, plese specify a different first_date that",
                     "represents a day within [1, 28]."
                     )
        msg <- paste(strwrap(msg), collapse = "\n")
        warning(sprintf(msg, fd), call. = FALSE)
      }
      if (the_interval == "year" && the_month == 2 && the_day == 29) {
        # The first date occurs on a leap day.
        msg <- paste("The first_date (%s) represents a day that does not",
                     "occur in all years. Because of this, bins may not",
                     "fall on the same day. To prevent this behavior, please",
                     "specify a first_date that represents a different day."
                     )
        msg <- paste(strwrap(msg), collapse = "\n")
        warning(sprintf(msg, fd), call. = FALSE)
      }
    }
  }
  seq(first_date, last_date, by = the_interval)
}
