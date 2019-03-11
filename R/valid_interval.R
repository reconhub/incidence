#' Return TRUE if the interval is a valid date character
#'
#' @param the_interval an interval string
#'
#' @return a logical value
#' @noRd
#' @keywords internal
is_date_interval <- function(the_interval) {
  valid_intervals <- "day|week|month|quarter|year"
  grepl(valid_intervals, the_interval)
}

#' Validate potential character values for interval
#'
#' Characters are valid for intervals if they are of the
#' form "day", "week", "month", etc. They can ALSO be
#' valid if they are characters that convert to numbers.
#'
#' @param the_interval a character string of length one
#'
#' @author Zhian Kamvar
#' @return the character string OR a numeric value.
#' @noRd
valid_interval_character <- function(the_interval) {
  if (is.character(the_interval)) {
    if (!is_date_interval(the_interval)) {
      suppressWarnings({
        the_interval <- as.numeric(the_interval)
      })
      if (is.na(the_interval)) {
        stop('The interval must be a number or one of the following: "day", "week", "month", "quarter" or "year"', 
             call. = FALSE)
      }
    }
  }
  the_interval
}

#' Check to make sure an interval is valid for integer dates
#'
#' This will try to convert the interval if its a character, but complain if
#' it doesn't pass check.
#'
#' @param interval either an integer or character
#'
#' @return interval or it will stop
#' @noRd
#' @keywords internal
valid_interval_integer <- function(interval) {
  if (is.character(interval)) {
    res <- try(valid_interval_character(interval), silent = TRUE)
    if (inherits(res, "try-error")) {
      msg <- sprintf("The interval '%s' is not valid. Please supply an integer.", interval)
      stop(msg, call. = FALSE)
    } else if (is.character(res)) {
      msg <- sprintf("The interval '%s' can only be used for Dates, not integers or numerics.",
                     interval)
      stop(msg, call. = FALSE)
    }
  }
  interval
}
