
## These functions are meant for internal use only, and are not
## exported. Functions which check content return it, after potential trivial
## conversions.


##' Default internal constructor for incidence objects.
##'
##'
##'
##' @param dates A vector of dates, which can be provided as objects of the
##' class: integer, numeric, Date, POSIXct. Note that decimal numbers will be
##' floored with a warning.
##'
##' @param interval An integer indicating the (fixed) size of the time interval
##' used for computing the incidence; defaults to 1 day.
##'
##' @param groups An optional factor defining groups of observations for which
##' incidence should be computed separately.
##'
##' @param na_as_group A logical value indicating if missing group (NA) should be
##' treated as a separate group.
##'
##' @param last_date The last date to be included in the produced epicurve. If
##'   \code{NULL} (default), the last date will be the most recent provided in
##'   \code{dates}.
##'
##' @param ... Additional arguments passed to other methods (none are used).
##'
##' @author Zhian Kamvar
##' @return an incidence object
##' @noRd
make_incidence <- function(dates, interval = 1L, groups = NULL,
                           na_as_group = TRUE,
                           last_date = NULL, ...) {
  dots     <- list(...)

  ## make sure input can be used
  dates    <- check_dates(dates)
  interval <- check_interval(interval)
  groups   <- check_groups(groups, dates, na_as_group)

  ## Check the interval and arrange the breaks
  breaks   <- make_breaks_easier(dates, interval, last_date, dots)

  ## compute counts within bins defined by the breaks
  if (!is.null(groups)) {
    counts <- tapply(dates, groups, count.dates, breaks)
    counts <- matrix(as.integer(unlist(counts, use.names = FALSE)),
                     ncol = length(levels(groups)))
    colnames(counts) <- levels(groups)
  } else {
    counts <- count.dates(dates, breaks)
    counts <- matrix(as.integer(counts), ncol = 1L)
  }

  out <- list(dates      = breaks,      # left side of bins (incl left, excl right)
              counts     = counts,      # computed incidence, 1 col / group
              timespan   = diff(range(breaks, na.rm = TRUE)) + 1,
              interval   = interval,    # fixed bin size
              n          = sum(counts), # total number of cases
              cumulative = FALSE)       # not cumulative at creation
  class(out) <- "incidence"
  out
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
#' @author Zhian Kamvar
#'
#' @noRd
make_breaks <- function(first_date, last_date, the_interval) {
  the_interval <- valid_interval_character(the_interval)
  seq(first_date, last_date, by = the_interval)
}

#' Make breaks with dates
#'
#' Because Date objects have a specific `seq` method, it's possible
#' to make breaks with both integers and date objects. This function
#' will check to make sure that the interval is valid.
#'
#' @param date an integer, numeric, or Date vector
#' @param the_interval an integer or character
#' @param last_date an integer, numeric, or Date
#' @param dots a named list of options
#'
#' @author Zhian Kamvar
#' @return a vector of integers or Dates
#' @noRd
#' @examples
#'
#' set.seed(999)
#' d <- sample(10, replace = TRUE)
#' make_breaks_easier(d, 2L)
make_breaks_easier <- function(dates, the_interval, last_date = NULL, dots = 1L) {
  ## check interval
  first_date  <- min(dates, na.rm = TRUE)
  if (is.null(last_date)) {
    last_date <- max(dates, na.rm = TRUE)
  }
  if (is.numeric(last_date)) {
    last_date <- as.integer(last_date)
  }
  if (!is.integer(last_date) && !inherits(last_date, "Date")) {
    stop("last_date not provided as an integer or Date", call. = FALSE)
  }
  if ("iso_week" %in% names(dots)) {
    is_a_week <- check_week(the_interval)
    if (is_a_week && identical(dots$iso_week, TRUE)) {
      first_isoweek <- ISOweek::date2ISOweek(first_date)
      substr(first_isoweek, 10, 10) <- "1"
      first_date <- ISOweek::ISOweek2date(first_isoweek)
    }
  }
  the_interval <- valid_interval_character(the_interval)
  seq(first_date, last_date, by = the_interval)
}

check_week <- function(the_interval) {
  num_week  <- is.numeric(the_interval) && the_interval == 7
  int_week  <- is.integer(the_interval) && the_interval == 7L
  char_week <- is.character(the_interval) && grepl(the_interval, "week")
  num_week || int_week || char_week
}

#' Count dates within bins
#'
#' @param dates a vector of dates, integers, or numerics
#' @param breaks an ordered vector of dates or integers
#'
#' @author Thibaut Jombart
#' @return an integer vector of the number of incidences per date
#' @noRd
#'
count.dates <- function(dates, breaks){
  counts <- table(cut(as.integer(dates), breaks = c(breaks, Inf), right = FALSE))
  as.integer(counts)
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





## A fix for the nonesensical behaviour of `sample` when first argument is of
## length 1.

sample_ <- function(x, ...) {
  x[sample.int(length(x), ...)]
}
