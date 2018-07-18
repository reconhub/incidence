
# Enforcers/Manipulators --------------------------------------------------
## This section of functions will attempt to manipulate the incoming object
## to the right format or die doing so.

## Non-exported function, enforces that 'groups' is either NULL or:
## - a factor
## - of the same length as 'dates'
##
## It also treats missing groups (NA) as a separate group is needed.

check_groups <- function(x, dates, na_as_group){
  if (is.null(x)) {
    return(NULL)
  }
  if (na_as_group) {
    x <- as.character(x)
    x[is.na(x)] <- "NA"
  }
  if (length(x) != length(dates)) {
    stop(sprintf(
      "'x' does not have the same length as dates (%d vs %d)",
      length(x), length(dates)))
  }
  factor(x)
}

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

## Non-exported function, enforces that an interval is:
## - strictly positive
## - integer (rounded) OR compatibile with date
## - finite
## - of length 1
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

## This function checks that usable dates are provided, and set non-finite
## values to NA. It also makes a few trivial conversions on the fly.

check_dates <- function(x, error_on_NA = FALSE, ...) {

  if (is.null(x)) {
    stop("dates is NULL", call. = FALSE)
  }

  if (is.character(x)) {
    x <- as.Date(x, ...)
  }

  not_finite <- !is.finite(x)
  if (sum(not_finite) > 0) {
    x[not_finite] <- NA
  }

  if (any(is.na(x)) && error_on_NA) {
    msg <- "NA detected in the dates"
    stop(msg, call. = FALSE)
  }

  if (sum(!is.na(x)) < 1) {
    stop("At least one (non-NA) date must be provided", call. = FALSE)
  }

  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(x)
  }

  if (is.integer(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    x_ori <- x
    x <- as.integer(floor(x))
    if (!isTRUE(note <- all.equal(x, x_ori))) {
      msg <- paste0(
        "Flooring from non-integer date caused approximations:\n",
        note)
      warning(msg, call. = FALSE)
    }
    return(x)
  }


  formats <- c("Date", "POSIXct", "integer", "numeric", "character")
  msg <- paste0(
    "Input could not be converted to date. Accepted formats are:\n",
    paste(formats, collapse = ", "))
  stop(msg)

}

# Trust but verify --------------------------------------------------------
## These functions trust that the input is of a given class, but will
## verify them and throw an error if it is not. No attempt at conversion
## will be made.


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
        stop('The interval must be a number or one of the following: "day", "week", "month", "quarter" or "year"', call. = FALSE)
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



# Returns Logical ---------------------------------------------------------
## These functions will simply return a logical value if they pass or fail
## the tests.

#' Return TRUE if the interval is a valid date character
#'
#' @param the_interval an interval string
#'
#' @return a logical value
#' @noRd
#' @keywords internal
is_date_interval <- function(the_interval) {
  valid_intervals <- c("day", "week", "month", "quarter", "year",
                       "days", "weeks", "months", "quarters", "years")
  the_interval %in% valid_intervals
}

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
  char_week <- is.character(the_interval) && grepl(the_interval, "week")
  num_week || int_week || char_week
}

