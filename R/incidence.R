##' Compute incidence of events from a vector of dates.
##'
##' This function computes incidence based on dates of events provided in
##' various formats. A fixed interval, provided as numbers of days, is used to
##' define time intervals. Counts within an interval always include the first
##' date, after which they are labelled, and exclude the second. For instance,
##' intervals labelled as 0, 3, 6, ... mean that the first bin includes days 0, 1
##' and 2, the second interval includes 3, 4 and 5 etc.
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
##' @param ... Additional arguments passed to other methods (none are used).
##'
##' @return An list with the class \code{incidence}, which contains the
##' following items:
##'
##' \itemize{
##'
##' \item dates: The dates marking the left side of the bins used for counting
##' events. When ISO week-based weekly incidence is computed, the dates are the
##' first days of corresponding isoweeks.
##'
##' \item counts: A matrix of incidence counts, which one column per group (and
##' a single column if no groups were used).
##'
##' \item timespan: The length of the period for which incidence is computed, in
##' days.
##'
##' \item interval: The bin size, in number of days; e.g. 7 indicates weekly
##' incidence.
##'
##' \item n: The total number of cases.
##'
##' \item isoweeks: ISO 8601 week format yyyy-Www, which is returned only when
##' ISO week-based weekly incidence is computed.
##'
##' }
##'
##' @details For details about the \code{incidence class}, see the dedicated
##' vignette:\cr \code{vignette("incidence_class", package = "incidence")}
##'
##' @seealso
##' The main other functions of the package include:
##' \itemize{
##'
##'  \item \code{\link{fit}}: Fit log-linear model to computed incidence.
##'
##'  \item \code{\link{fit_optim_split}}: Find the optimal peak of the epidemic
##' and fits log-linear models on either side of the peak.
##'
##'  \item \code{\link[incidence]{subset}}: Handling of \code{incidence}
##' objects.
##'
##'  \item \code{\link{pool}}: Sum incidence over groups.
##'
##'  \item \code{\link{as.data.frame}}: Convert an \code{incidence} object to a
##' \code{data.frame}.}
##'
##' The following vignettes are also available:
##' \itemize{
##'
##' \item \code{overview}: Provides an overview of the package's features.
##'
##' \item \code{customize_plot}: Provides some tips on finer plot customization.
##'
##' \item \code{incidence_class}: Details the content of the \code{incidence}
##' class.
##'
##' }
##'
##'
##'
##' @author Thibaut Jombart, Rich Fitzjohn
##'
##' @rdname incidence
##'
##' @importFrom utils head tail
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
##'   onset <- ebola_sim$linelist$date_of_onset
##'
##'   ## daily incidence
##'   inc <- incidence(onset)
##'   inc
##'   plot(inc)
##'
##'   ## weekly incidence
##'   inc.week <- incidence(onset, interval = 7, iso_week = FALSE)
##'   inc.week
##'   plot(inc.week)
##'   plot(inc.week, border = "white") # with visible border
##'   inc.isoweek <- incidence(onset, interval = 7, iso_week = TRUE)
##'   inc.isoweek
##'   ## use group information
##'   sex <- ebola_sim$linelist$gender
##'   inc.week.gender <- incidence(onset, interval = 7, groups = sex, iso_week = FALSE)
##'   inc.week.gender
##'   head(inc.week.gender$counts)
##'   plot(inc.week.gender)
##'   inc.isoweek.gender <- incidence(onset, interval = 7, groups = sex, iso_week = TRUE)
##'   inc.isoweek.gender
##' }
##'
##'
incidence <- function(dates, interval = 1L, ...) {
  UseMethod("incidence")
}





## The default incidence is designed for dates provided as integers, and a fixed
## time interval defaulting to 1. 'bins' are time intervals, identified by the
## left date, left-inclusive and right-exclusive, i.e. the time interval defined
## by d1 and d2 is [d1, d2[.

##' @export
##' @rdname incidence
##'
##' @param na_as_group A logical value indicating if missing group (NA) should be
##' treated as a separate group.
##'
##' @param last_date The last date to be included in the produced epicurve. If
##'   \code{NULL} (default), the last date will be the most recent provided in
##'   \code{dates}.
##'

incidence.integer <- function(dates, interval = 1L, groups = NULL,
                              na_as_group = TRUE,
                              last_date = NULL, ...) {
  dots <- list(...)
  ## make sure input can be used
  dates <- check_dates(dates)
  interval <- check_interval(interval)
  groups <- check_groups(groups, dates, na_as_group)

  ## check interval
  first_date <- min(dates, na.rm = TRUE)
  if (is.null(last_date)) {
    last_date <- max(dates, na.rm = TRUE)
  } else if (!is.integer(last_date)) {
    stop("last_date not provided as an integer")
  }

  interval <- as.integer(round(interval))
  if ("iso_week" %in% names(dots)) {
    if (interval == 7L && dots$iso_week == TRUE) {
      first_date <- 0L
    }
  }

  ## function to compute counts of dates with defined breaks
  count.dates <- function(dates, breaks){
    counts <- table(cut(as.integer(dates), breaks = c(breaks, Inf), right = FALSE))
    as.integer(counts)
  }


  ## define breaks here
  breaks <- seq(first_date, last_date, by = interval) # 'd1' in expl above
  breaks <- as.integer(breaks)

  ## compute counts within bins defined by the breaks
  if (!is.null(groups)) {
    counts <- tapply(dates, groups, count.dates, breaks)
    counts <- matrix(as.integer(unlist(counts)),
                     ncol = length(levels(groups)))
    colnames(counts) <- levels(groups)
  } else {
    counts <- count.dates(dates, breaks)
    counts <- matrix(as.integer(counts), ncol = 1L)
  }

  out <- list(dates = breaks, # left side of bins (incl left, excl right)
              counts = counts, # computed incidence, 1 col / group
              timespan = diff(range(breaks, na.rm = TRUE)) + 1,
              interval = interval, # fixed bin size
              n = sum(counts)) # total number of cases
  class(out) <- "incidence"
  out
}




##' @export
##' @rdname incidence

incidence.default <- incidence.integer





##' @export
##' @rdname incidence

incidence.numeric <- function(dates, interval = 1L, ...) {
  ## make sure input can be used

  dates <- as.integer(check_dates(dates))
  out <- incidence.integer(dates, interval, ...)
  out$dates <- as.numeric(out$dates)
  out
}





##' @export
##' @rdname incidence
##' @param iso_week A logical value indicating if the returning \code{incidence}
##'   should be ISO week-based when computing weekly incidence (interval =
##'   7). defaults to be TRUE.

incidence.Date <- function(dates, interval = 1L, iso_week = TRUE,
                           last_date = NULL, ...) {
  ## make sure input can be used
  dates <- check_dates(dates)
  stopifnot(is.logical(iso_week))

  first_date <- min(dates, na.rm = TRUE)
  if (is.null(last_date)) {
    last_date <- max(dates, na.rm = TRUE)
  } else {
    if (!inherits(last_date, "Date")) {
      stop("last_date is not a Date object")
    }
  }
  interval <- as.integer(round(interval))

  if (interval == 7L && iso_week) {
    first_isoweek <- ISOweek::date2ISOweek(first_date)
    substr(first_isoweek, 10, 10) <- "1"
    first_date <- ISOweek::ISOweek2date(first_isoweek)
  }

  dates_int <- as.integer(dates - first_date)
  last_date_int <- as.integer(last_date - first_date)

  out <- incidence.integer(dates_int,
                           interval = interval,
                           iso_week = iso_week,
                           last_date = last_date_int,
                           ...)

  out$dates <- first_date + out$dates
  if (interval == 7L && iso_week) {
    # dates are the first days of corresponding ISOweeks.
    out$isoweeks <- substr(ISOweek::date2ISOweek(out$dates), 1, 8)
  }

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
##' @param x An 'incidence' object.

print.incidence <- function(x, ...) {
  cat("<incidence object>\n")
  cat(sprintf("[%d cases from days %s to %s]\n",
              sum(x$n), min(x$dates), max(x$dates)))
  if (x$interval == 7L && "isoweeks" %in% names(x)) {
    cat(sprintf("[%d cases from ISO weeks %s to %s]\n",
                sum(x$n), head(x$isoweeks, 1), tail(x$isoweeks, 1)))
  }
  if (ncol(x$counts) > 1L) {
    groups.txt <- paste(colnames(x$counts), collapse = ", ")
    cat(sprintf("[%d groups: %s]\n", ncol(x$counts), groups.txt))
  }
  cat(sprintf("\n$counts: matrix with %d rows and %d columns\n",
              nrow(x$counts), ncol(x$counts)))
  cat(sprintf("$n: %d cases in total\n", x$n))
  cat(sprintf("$dates: %d dates marking the left-side of bins\n",
              length(x$dates)))
  cat(sprintf("$interval: %d %s\n",
              x$interval, ifelse(x$interval < 2, "day", "days")))
  cat(sprintf("$timespan: %d days\n\n", x$timespan))
  invisible(x)
}




