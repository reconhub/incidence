##' Compute incidence from a numerical vector of dates, representing
##' days.
##'
##' This function computes incidence based on dates of events provided in various formats. A fixed
##' interval, provided as numbers of days, is used to define time intervals. Counts within an
##' interval always include the first date, after which they are labelled, and exclude the
##' second. For instance, intervals labelled as##' 0,3,6,... mean that the first bin includes days
##' 0, 1 and 2, the second interval includes 3, 4 and 5m etc.
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
##' @author Rich Fitzjohn, Thibaut Jombart
##'
##' @rdname incidence
##'
##' @export
##'
##' @examples
##' ## toy example
##' incidence(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2))
##' incidence(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2), 2)
##'
##' ## example using simulated dataset
##' if(require(outbreaks)) {
##'   onset <- ebola.sim$linelist$date.of.onset
##'
##'   ## daily incidence
##'   inc <- incidence(onset)
##'   inc
##'   plot(inc)
##'
##'   ## weekly incidence
##'   inc.week <- incidence(onset, interval = 7)
##'   inc.week
##'   plot(inc.week)
##'   plot(inc.week, border = "white") # with visible border
##' }
##'
##'
incidence <- function(dates, interval = 1L, ...) {
  UseMethod("incidence")
}





## The default incidence is designed for dates provided as integers, and a fixed time
## interval defaulting to 1. 'bins' are time intervals, identified by the left date, left-inclusive
## and right-exclusive, i.e. the time interval defined by d1 and d2 is [d1, d2[.

##' @export
##' @rdname incidence

incidence.integer <- function(dates, interval = 1L, ...) {
    ## make sure input can be used
    dates <- check.dates(dates)

    first.date <- min(dates)
    last.date <- max(dates)
    interval <- round(interval)

    ## handle case where interval is larger than span
    if (last.date-first.date < interval){
        breaks <- as.integer(first.date)
        counts <- length(dates)
    } else {
        breaks <- seq(first.date, last.date, by=interval) # these are 'd1' in expl above
        breaks <- as.integer(breaks)
        counts <- as.integer(table(cut(as.integer(dates), breaks=c(breaks, Inf), right=FALSE)))
    }

    out <- list(dates = breaks, # left side of the intervals (incl left, excl right)
                counts = matrix(counts, ncol = 1L), # counts; add columns for stratif incid
                timespan = diff(range(dates, na.rm=TRUE))+1, # time span (last date - first date + 1 day)
                interval = interval, # fixed bin size
                n = sum(counts)) # total number of cases
    class(out) <- "incidence"
    out
}




##'
##' @export
##' @rdname incidence
##'
incidence.default <- incidence.integer





##'
##' @export
##' @rdname incidence
##'
incidence.numeric <- function(dates, interval = 1L, ...) {
    ## make sure input can be used
    dates <- check.dates(dates)

    message("Dates stored as decimal numbers were floored.\n")
    dates <- as.integer(floor(dates))
    out <- incidence.integer(dates, interval, ...)
    out$dates <- as.numeric(out$dates)
    out
}





##' @export
##' @rdname incidence

incidence.Date <- function(dates, interval = 1L, ...) {
    ## make sure input can be used
    dates <- check.dates(dates)

    first.date <- min(dates, na.rm = TRUE)
    out <- incidence.default(as.integer(dates - first.date), interval, ...)
    out$dates <- first.date + out$dates
    out
}






##' @export
##' @rdname incidence

incidence.POSIXt <- function(dates, ...) {
    ## make sure input can be used
    dates <- check.dates(dates)

    ret <- incidence(as.Date(dates))
    f <- if (inherits(dates, "POSIXct")) as.POSIXct else as.POSIXlt
    ret$dates <- f(ret$dates)
    ret
}





##' @export
##' @rdname incidence
##' @param x an 'incidence' object
print.incidence <- function(x, ...) {

  cat("<incidence object>\n")
  cat(sprintf("[%d cases from days %s to %s]\n\n", sum(x$n), min(x$dates), max(x$dates)))
  cat(sprintf("$counts: matrix with %d rows and %d columns\n",
              nrow(x$counts), ncol(x$counts)))
  cat(sprintf("$n: %d cases in total\n", x$n))
  cat(sprintf("$dates: %d dates marking the left-side of bins\n", length(x$dates)))
  cat(sprintf("$interval: %d %s\n", x$interval, ifelse(x$interval<2, "day", "days")))
  cat(sprintf("$timespan: %d days\n\n", x$timespan))
  invisible(x)
}





## This function is non-exported; it merely checks that usable data are provided, and returns data
## without NAs.

check.dates <- function(dates){
    ## make sure input can be used
    to.remove <- !is.finite(dates)
    if (sum(to.remove)>0) {
        message(sprintf("%d non-finite values (NA, Inf) where removed from the data.\n", sum(to.remove)))
        dates <- dates[!to.remove]

    }

    if (length(dates) < 1) {
        stop("At least one (non-NA) date must be provided")
    }

    dates
}
