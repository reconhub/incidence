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
##' @param groups An optional factor defining groups of observations for which incidence should be
##' computed separately.
##'
##' @param ... Additional arguments passed to other methods (none are used).
##'
##' @return A named list with 2 members (days [first day per interval] and
##'   NoOfCases [count of incidents during the respective interval])
##'
##' @author Thibaut Jombart, Rich Fitzjohn
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
##'   ## use group information
##'   sex <- ebola.sim$linelist$gender
##'   inc.week.gender <- incidence(onset, interval = 7, groups = sex)
##'   inc.week.gender
##'   head(inc.week.gender$counts)
##'   plot(inc.week.gender)
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
##' @param na_as_group A logical indicating if missing group (NA) should be treated as a separate
##' group.

incidence.integer <- function(dates, interval = 1L, groups = NULL, na_as_group = TRUE, ...) {
    ## make sure input can be used
    dates <- check_dates(dates)
    interval <- check_interval(interval) # enforces positive, finite integer
    groups <- check_groups(groups, dates, na_as_group) # enforces factor of right length

    ## check interval
    first.date <- min(dates)
    last.date <- max(dates)
    interval <- as.integer(round(interval))

    ## function to compute counts of dates with defined breaks
    count.dates <- function(dates, breaks){
        counts <- table(cut(as.integer(dates), breaks=c(breaks, Inf), right=FALSE))
        as.integer(counts)
    }

    ## define breaks here
    breaks <- seq(first.date, last.date, by=interval) # these are 'd1' in expl above
    breaks <- as.integer(breaks)

    ## compute counts within bins defined by the breaks
    if (!is.null(groups)) {
        counts <- tapply(dates, groups, count.dates, breaks)
        counts <- matrix(as.integer(unlist(counts)), ncol = length(levels(groups)))
        colnames(counts) <- levels(groups)
    } else {
        counts <- count.dates(dates, breaks)
        counts <- matrix(as.integer(counts), ncol = 1L)
    }

    out <- list(dates = breaks, # left side of the intervals (incl left, excl right)
                counts = counts, # counts; add columns for stratif incid
                timespan = diff(range(breaks, na.rm=TRUE))+1, # time span (last date - first date + 1 day)
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
    dates <- check_dates(dates)

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
    dates <- check_dates(dates)

    first.date <- min(dates, na.rm = TRUE)
    out <- incidence.integer(as.integer(dates - first.date), interval, ...)
    out$dates <- first.date + out$dates
    out
}






##' @export
##' @rdname incidence

incidence.POSIXt <- function(dates, interval = 1L, ...) {
    ## make sure input can be used
    dates <- check_dates(dates)

    ret <- incidence(as.Date(dates), interval, ...)
    f <- if (inherits(dates, "POSIXct")) as.POSIXct else as.POSIXlt
    ret$dates <- f(ret$dates)
    ret
}





##' @export
##' @rdname incidence
##' @param x an 'incidence' object
print.incidence <- function(x, ...) {

  cat("<incidence object>\n")
  cat(sprintf("[%d cases from days %s to %s]\n", sum(x$n), min(x$dates), max(x$dates)))
  if (ncol(x$counts) > 1L) {
      groups.txt <- paste(colnames(x$counts), collapse = ", ")
      cat(sprintf("[%d groups: %s]\n", ncol(x$counts), groups.txt))
  }
  cat(sprintf("\n$counts: matrix with %d rows and %d columns\n",
              nrow(x$counts), ncol(x$counts)))
  cat(sprintf("$n: %d cases in total\n", x$n))
  cat(sprintf("$dates: %d dates marking the left-side of bins\n", length(x$dates)))
  cat(sprintf("$interval: %d %s\n", x$interval, ifelse(x$interval<2, "day", "days")))
  cat(sprintf("$timespan: %d days\n\n", x$timespan))
  invisible(x)
}





## This function is non-exported; it merely checks that usable data are provided, and returns data
## without NAs.

check_dates <- function(dates){
    ## make sure input can be used
    to.remove <- !is.finite(dates)
    if (sum(to.remove)>0) {
        message(sprintf("%d non-finite values (NA, Inf) where removed from the data.\n",
                        sum(to.remove)))
        dates <- dates[!to.remove]

    }

    if (length(dates) < 1) {
        stop("At least one (non-NA) date must be provided")
    }

    dates
}





## Non-exported function, enforces that an interval is:
## - strictly positive
## - integer (rounded)
## - finite
## - of length 1
check_interval <- function(interval){
    if (missing(interval) || is.null(interval)) {
        stop("Interval is missing or NULL")
    }
    if (length(interval) != 1L) {
        stop(sprintf("Exactly one value should be provided as interval (%d provided)",
             length(interval)))
    }
    if (!is.finite(interval)) {
        stop("interval is not finite")
    }
    interval <- as.integer(round(old <- interval))
    if (interval < 1L) {
        stop(sprintf("interval must be at least 1 (input: %.3f; after rounding: %d)",
                     old, interval))
    }
    interval
}





## Non-exported function, enforces that 'groups' is either NULL or:
## - a factor
## - of the same length as 'dates'
##
## It also treats missing groups (NA) as a separate group is needed.
##
check_groups <- function(groups, dates, na_as_group){
    if (is.null(groups)) {
        return(NULL)
    }
    if (na_as_group) {
        groups <- as.character(groups)
        groups[is.na(groups)] <- "NA"
    }
    if (length(groups) != length(dates)) {
        stop(sprintf("'groups' does not have the same length as dates (%d vs %d)",
             length(groups), length(dates)))
    }
    factor(groups)
}

