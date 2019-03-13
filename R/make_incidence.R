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
  interval <- check_interval(interval, if (is.null(dots$standard)) TRUE else dots$standard)
  groups   <- check_groups(groups, dates, na_as_group)

  ## Check the interval and arrange the breaks
  first_date <- check_boundaries(dates, first_date, "first")
  last_date  <- check_boundaries(dates, last_date, "last")
  breaks     <- make_breaks_easier(dates,
                                   the_interval    = interval,
                                   first_date      = first_date,
                                   last_date       = last_date,
                                   dots            = dots
                                   )
  if (!is.numeric(interval) && grepl("week", interval)) {
    interval <- get_week_duration(interval)
  }

  ## Trim the dates and groups as necessary
  trimmed <- trim_observations(dates, first_date, last_date)
  dates   <- dates[trimmed]
  groups  <- groups[trimmed]

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
