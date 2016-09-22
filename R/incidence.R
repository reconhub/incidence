##' Compute incidence from a numerical vector of dates, representing
##' days.
##'
##' This function takes date of symptom dates, per case, as input which
##' is in the format of number of days after a reference date. Each row
##' represents a case, and the reference day could be the day of the
##' oubreak, if known. 0 would suggest the case developed symptoms on
##' the reference date, 1 would mean the day after the reference date,
##' etc. The number of incidences per interval (in days) are then
##' computed, for which the default length of the interval is 1
##' day. The output is a named list with a member for the interval
##' (Days - same format as the input of dates) and a member for the
##' number of incidences in that interval. The interval is denoted by
##' the first day in the interval, i.e. if the list of the interval is
##' 0,3,6,... then the interval is of length 3 and days 0, 1 and 2 are
##' in the first interval, days 3, 4 and 5 are in the seconde, etc.
##'
##' @param dates A vector of dates, which can be provided as objects of the class: integer, numeric,
##' Date, POSIXct. Note that decimal numbers will be floored with a warning.
##'
##' @param interval An integer indicating the (fixed) size of the time interval used for computing
##' the incidence; defaults to 1 day.
##'
##' @param ... Additional arguments passed to other methods (none are used).
##'
##' @return A named list with 2 members (days [first day per interval] and
##'   NoOfCases [count of incidents during the respective interval])
##'
##' @export
##' @examples
##' incidence(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2))
##' incidence(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2), 2)
incidence <- function(dates, interval = 1, ...) {
  UseMethod("incidence")
}

## TODO:
## Support:
## * incidence.Date
## * incidence.POSIXt (both ct and lt)
## * matrices, etc; give a vector of results back?
## * support as.zoo, as.ts, etc.

##' @export
incidence.default <- function(dates, interval = 1, ...) {
    if(interval > 1) {
        stop("Interval > 1 not supported.")
    }
  first_day <- min(dates)
  if (first_day != 1L) {
    dates <- dates - first_day + 1L
  }
  last_day <- max(dates)
  counts <- tabulate(dates, last_day)

  day <- seq_along(counts)
  if (first_day != 1L) {
    day <- day + first_day - 1L
  }

  ret <- list(incidence = data.frame(dates = day, counts = counts),
              timespan = range(day),
              interval = interval,
              n = sum(counts))
  class(ret) <- "incidence"
  ret
}

##' @export
print.incidence <- function(x, ...) {

  cat("<incidence object>\n")
  cat(sprintf("[%d cases from days %s to %s]\n\n", sum(x$n), x$timespan[1], x$timespan[2]))
  cat(sprintf("$incidence: data.frame with %d rows and %d columns (%s)\n",
              nrow(x$incidence), ncol(x$incidence),
              paste(names(x$incidence), collapse = ", ")))
  cat(sprintf("$interval: %d %s\n", x$interval, ifelse(x$interval<2, "day", "days")))
  cat(sprintf("$n: %d cases in total\n", x$n))
  invisible(x)
}
