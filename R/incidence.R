##' Compute incidence from a numerical vector of onsets, representing
##' days.
##'
##' This function takes date of symptom onset, per case, as input which
##' is in the format of number of days after a reference date. Each row
##' represents a case, and the reference day could be the day of the
##' oubreak, if known. 0 would suggest the case developed symptoms on
##' the reference date, 1 would mean the day after the reference date,
##' etc. The number of incidences per interval (in days) are then
##' computed, for which the default length of the interval is 1
##' day. The output is a named list with a member for the interval
##' (Days - same format as the input of onset) and a member for the
##' number of incidences in that interval. The interval is denoted by
##' the first day in the interval, i.e. if the list of the interval is
##' 0,3,6,... then the interval is of length 3 and days 0, 1 and 2 are
##' in the first interval, days 3, 4 and 5 are in the seconde, etc.
##'
##' @param onset An numerical vector, each element of which
##'   corresponds to a case.  The vector does not need to be sorted,
##'   but must be positive?
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
incidence <- function(onset, ...) {
  UseMethod("incidence")
}

## TODO:
## Support:
## * incidence.Date
## * incidence.POSIXt (both ct and lt)
## * matrices, etc; give a vector of results back?
## * support as.zoo, as.ts, etc.

##' @export
incidence.default <- function(onset, ...) {
  first_day <- min(onset)
  if (first_day != 1L) {
    onset <- onset - first_day + 1L
  }
  last_day <- max(onset)
  cases <- tabulate(onset, last_day)

  day <- seq_along(cases)
  if (first_day != 1L) {
    day <- day + first_day - 1L
  }

  ret <- list(day = day, cases = cases)
  class(ret) <- "incidence"
  ret
}

##' @export
print.incidence <- function(x, ...) {
  cat("<incidence object>\n")
  cat(sprintf("  - %d cases\n", sum(x$cases)))
  r <- range(x$day)
  if (as.integer(r[[1L]]) == 1L) {
    cat(sprintf("  - Spanning %d days\n", diff(r)))
  } else {
    cat(sprintf("  - Spanning %d days [%s to %s]\n", diff(r), r[[1L]], r[[2L]]))
  }
  invisible(x)
}
