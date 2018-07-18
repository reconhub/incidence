
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
##'   `NULL` (default), the last date will be the most recent provided in
##'   `dates`.
##'
##' @param ... Additional arguments passed to other methods (none are used).
##'
##' @author Zhian Kamvar
##' @return an incidence object
##' @noRd
make_incidence <- function(dates, interval = 1L, groups = NULL,
                           na_as_group = TRUE, first_date = NULL,
                           last_date = NULL, ...) {
  dots     <- list(...)

  ## make sure input can be used
  dates    <- check_dates(dates)
  interval <- check_interval(interval)
  groups   <- check_groups(groups, dates, na_as_group)

  ## Check the interval and arrange the breaks
  null_first_date <- is.null(first_date)
  first_date      <- check_boundaries(dates, first_date, "first")
  last_date       <- check_boundaries(dates, last_date, "last")
  breaks          <- make_breaks_easier(dates,
                                        the_interval    = interval,
                                        first_date      = first_date,
                                        last_date       = last_date,
                                        dots            = dots,
                                        null_first_date = null_first_date
                                        )

  ## Trim the dates
  dates <- trim_dates(dates, first_date, last_date)

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

#' Trim dates based on the first and last dates
#'
#' @param dates a vector of dates or integers
#' @param first_date a single date or integer
#' @param last_date a single date or integer
#'
#' @return the trimmed dates
#' @noRd
#' @keywords internal
trim_dates <- function(dates, first_date = NULL, last_date = NULL) {
  res <- dates[dates >= first_date & dates <= last_date]
  if (length(res) < length(dates)) {
    warning(sprintf("I removed %d observations outside of [%s, %s].",
                    length(dates) - length(res),
                    format(first_date),
                    format(last_date)
                    ),
            call. = FALSE
            )
  }
  res
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
        # This returns something like 2018-W29-2, where the last digit indicates
        # the day of the week
        first_isoweek <- ISOweek::date2ISOweek(first_date)
        # Here, we force it to be the start of the week
        substr(first_isoweek, 10, 10) <- "1"
        # and convert it back
        first_date <- ISOweek::ISOweek2date(first_isoweek)
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

## This function takes a vector of Date objects, and an ideal number of breaks,
## and generates a list with two components: $breaks, and $labels. $breaks
## correspond to the first day of the matching iso week; $labels contains vector
## of labels of the corresponding iso weeks.

make_iso_breaks <- function(dates, n = 5) {
  breaks_ini <- pretty(dates, n)
  iso <- ISOweek::date2ISOweek(breaks_ini)
  iso_day1 <- sub("-[1-7]+$", "-1", iso)
  list(breaks = ISOweek::ISOweek2date(iso_day1),
       labels = sub("-[1-7]+$", "", iso)
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



## quantiles for Date objects

quantile_Date <- function(x, ...) {
  if (!inherits(x, "Date")) {
    stop("'x' is not a 'Date' object")
  }

  first_date <- min(x, na.rm = TRUE)
  x_num <- as.numeric(x - min(x))
  out <- stats::quantile(x_num, ...)
  first_date + out
}
