
## These functions are meant for internal use only, and are not
## exported. Functions which check content return it, after potential trivial
## conversions.






## This function checks that usable dates are provided, and set non-finite
## values to NA. It also makes a few trivial conversions on the fly.

check_dates <- function(x, error_on_NA = FALSE, ...) {
  if (is.null(x)) {
    stop("dates is NULL")
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
    stop(msg)
  }

  if (sum(!is.na(x)) < 1) {
    stop("At least one (non-NA) date must be provided")
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
      warning(msg)
    }
    return(x)
  }


  formats <- c("Date", "POSIXct", "integer", "numeric", "character")
  msg <- paste0(
    "Input could not be converted to date. Accepted formats are:\n",
    paste(formats, collapse = ", "))
  stop(msg)

}






## Non-exported function, enforces that an interval is:
## - strictly positive
## - integer (rounded)
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
    # TODO: Add criteria for Date intervals
    #
    stop("Interval is not finite")
  }
  x <- as.integer(round(old <- x)) # TODO: remove this force
  if (x < 1L) {
    stop(sprintf(
      "Interval must be at least 1 (input: %.3f; after rounding: %d)",
      old, x))
  }
  x
}


#' Validate potential character values for interval
#'
#' Characters are valid for intervals if they are of the
#' form "day", "week", "month", etc. They can ALSO be
#' valid if they are characters that convert to numbers.
#'
#' @param the_interval a character string of length one
#'
#' @return the character string OR a numeric value.
#' @noRd
valid_interval_character <- function(the_interval) {
  valid_intervals <- c("day", "week", "month", "quarter", "year",
                       "days", "weeks", "months", "quarters", "years")
  if (is.character(the_interval)) {
    if (!the_interval %in% valid_intervals) {
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


#' Make breaks with dates
#'
#' Because Date objects have a specific `seq` method, it's possible
#' to make breaks with both integers and date objects. This function
#' will check to make sure that the interval is valid.
#'
#' @param first_date an integer, numeric, or Date
#' @param last_date an integer, numeric, or Date
#' @param the_interval an integer or character
#'
#' @return
#' @noRd
make_breaks <- function(first_date, last_date, the_interval) {
  the_interval <- valid_interval_character(the_interval)
  seq(first_date, last_date, by = the_interval)
}



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




## This function takes a vector of Date objects, and an ideal number of breaks,
## and generates a list with two components: $breaks, and $labels. $breaks
## correspond to the first day of the matching iso week; $labels contains vector
## of labels of the corresponding iso weeks.

make_iso_weeks_breaks <- function(dates, n = 5) {
  breaks_ini <- pretty(dates, n)
  iso_weeks <- ISOweek::date2ISOweek(breaks_ini)
  iso_weeks_day1 <- sub("-[1-7]+$", "-1", iso_weeks)
  list(breaks = ISOweek::ISOweek2date(iso_weeks_day1),
       labels = sub("-[1-7]+$", "", iso_weeks)
       )
}




## Implement isTRUE and isFALSE to avoid dep on R 3.5.0

isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

isTRUE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}
