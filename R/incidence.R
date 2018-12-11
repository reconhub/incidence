#' Compute incidence of events from a vector of dates.
#'
#' This function computes incidence based on dates of events provided in
#' various formats. A fixed interval, provided as numbers of days, is used to
#' define time intervals. Counts within an interval always include the first
#' date, after which they are labeled, and exclude the second. For instance,
#' intervals labeled as 0, 3, 6, ... mean that the first bin includes days 0, 1
#' and 2, the second interval includes 3, 4 and 5 etc.
#'
#' @param dates A vector of dates, which can be provided as objects of the
#' class: integer, numeric, Date, POSIXct. Note that decimal numbers will be
#' floored with a warning.
#'
#' @param interval An integer or character indicating the (fixed) size of the time interval
#' used for computing the incidence; defaults to 1 day. This can also be a text string that corresponds to a valid date
#' interval: day, week, month, quarter, or year. See Note.
#'
#' @param groups An optional factor defining groups of observations for which
#' incidence should be computed separately.
#'
#' @param na_as_group A logical value indicating if missing group (NA) should be
#' treated as a separate group.
#'
#' @param first_date,last_date optional first/last dates to be used in the
#'   epicurve. When these are `NULL` (default), the dates from the first/last
#'   dates are taken from the observations. If these dates are provided, the
#'   observations will be trimmed to the range of \[first_date, last_date\].
#'
#' @param ... Additional arguments passed to other methods (none are used).
#'
#' @return An list with the class `incidence`, which contains the
#' following items:
#'
#'
#' - **dates**: The dates marking the left side of the bins used for counting
#' events. When ISO week-based weekly incidence is computed, the dates are the
#' first days of corresponding isoweeks.
#'
#' - **counts**: A matrix of incidence counts, which one column per group (and
#' a single column if no groups were used).
#'
#' - **timespan**: The length of the period for which incidence is computed, in
#' days.
#'
#' - **interval**: The bin size, in number of days; e.g. 7 indicates weekly
#' incidence.
#'
#' - **n**: The total number of cases.
#'
#' - **isoweeks**: ISO 8601 week format yyyy-Www, which is returned only when
#' ISO week-based weekly incidence is computed.
#'
#'
#' @details For details about the `incidence class`, see the dedicated
#' vignette:\cr `vignette("incidence_class", package = "incidence")`
#'
#' @note If `interval` is a valid character (e.g. "week" or "month"), then
#' the bin will start at the beginning of the interval just before the first
#' observation by default. For example, if the first case was recorded on
#' Wednesday, 2018-05-09:
#'
#'  - "week"    : first day of the ISOweek (i.e. Monday, 2018-05-07)
#'  - "month"   : first day of the month (i.e. 2018-05-01)
#'  - "quarter" : first day of the quarter (i.e. 2018-04-01)
#'  - "year"    : first day of the calendar year (i.e. 2018-01-01)
#'
#' These default intervals can be overridden in two ways:
#'
#'  1. Specify `standard = FALSE`, which sets the interval to begin at the first
#'     observed case.
#'  2. Specify a date in the `first_date` field.
#'
#' The intervals for "month", "quarter", and "year" will necessarily vary in the
#' number of days they encompass and warnings will be generated when the first
#' date falls outside of a calendar date that is easily represented across the
#' interval.
#'
#' @seealso
#' The main other functions of the package include:
#'
#'  - [incidence::plot.incidence()]: Plot epicurves from an incidence object.
#'
#'  - [incidence::fit()]: Fit log-linear model to computed incidence.
#'
#'  - [incidence::fit_optim_split()]: Find the optimal peak of the epidemic
#' and fits log-linear models on either side of the peak.
#'
#'  - [incidence::subset()]: Handling of `incidence`
#' objects.
#'
#'  - [incidence::pool()]: Sum incidence over groups.
#'
#'  - [incidence::as.data.frame.incidence()]: Convert an `incidence` object to a
#' `data.frame`.
#'
#' The following vignettes are also available:
#'
#' - `overview`: Provides an overview of the package's features.
#'
#' - `customize_plot`: Provides some tips on finer plot customization.
#'
#' - `incidence_class`: Details the content of the `incidence`
#' class.
#'
#'
#'
#'
#' @author Thibaut Jombart, Rich Fitzjohn, Zhian Kamvar
#'
#' @rdname incidence
#'
#' @importFrom utils head tail
#'
#' @export
#'
#' @examples
#' ## toy example
#' incidence(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2))
#' incidence(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2), 2)
#'
#' ## example using simulated dataset
#' if(require(outbreaks)) { withAutoprint({
#'   onset <- ebola_sim$linelist$date_of_onset
#'
#'   ## daily incidence
#'   inc <- incidence(onset)
#'   inc
#'   plot(inc)
#'
#'   ## weekly incidence
#'   inc.week <- incidence(onset, interval = 7, standard = FALSE)
#'   inc.week
#'   plot(inc.week)
#'   plot(inc.week, border = "white") # with visible border
#'   inc.isoweek <- incidence(onset, interval = 7, standard = TRUE)
#'   inc.isoweek
#'   ## use group information
#'   sex <- ebola_sim$linelist$gender
#'   inc.week.gender <- incidence(onset, interval = 7,
#'                                groups = sex, standard = FALSE)
#'   inc.week.gender
#'   head(inc.week.gender$counts)
#'   plot(inc.week.gender)
#'   inc.isoweek.gender <- incidence(onset, interval = 7,
#'                                   groups = sex, standard = TRUE)
#'   inc.isoweek.gender
#' })}
#'
#'
incidence <- function(dates, interval = 1L, ...) {
  UseMethod("incidence")
}


#' @export
#' @rdname incidence
incidence.default <- function(dates, interval = 1L, ...) {
  if (is.character(dates)) {
    stop('Input is a character. Did you forget to convert to Date?')
  }
  check_dates(dates)
}

#' @export
#' @rdname incidence
#'
#' @param standard (Only applicable to Date objects) When `TRUE` (default) and the
#'   `interval` one of "week", "month", "quarter", or "year", then this will
#'   cause the bins for the counts to start at the beginning of the interval
#'   (See Note). This is overridden by defining a non-NULL `first_date`.

incidence.Date <- function(dates, interval = 1L, standard = TRUE, groups = NULL,
                           na_as_group = TRUE, first_date = NULL,
                           last_date = NULL, ...) {
  dots <- check_dots(list(...), names(formals(incidence.Date)))
  ## make sure input can be used
  if (!is.logical(standard)) {
    stop("The argument `standard` must be either `TRUE` or `FALSE`.")
  }
  if ("standard" %in% names(dots)) {
    # the user specified iso_week and was given a warning.
    standard <- dots$standard
  }
  out <- make_incidence(dates = dates,
                        interval = interval,
                        groups = groups,
                        na_as_group = na_as_group,
                        first_date = first_date,
                        last_date = last_date,
                        standard = standard,
                        ...)
  if (check_week(interval) && standard) {
    # dates are the first days of corresponding ISOweeks.
    out$isoweeks <- substr(ISOweek::date2ISOweek(out$dates), 1, 8)
  }

  out
}


## The default incidence is designed for dates provided as integers, and a fixed
## time interval defaulting to 1. 'bins' are time intervals, identified by the
## left date, left-inclusive and right-exclusive, i.e. the time interval defined
## by d1 and d2 is [d1, d2[.

#' @export
#' @rdname incidence

incidence.integer <- function(dates, interval = 1L, groups = NULL,
                              na_as_group = TRUE, first_date = NULL,
                              last_date = NULL, ...) {
  dots <- check_dots(list(...), names(formals(incidence.integer)))
  interval <- valid_interval_integer(interval)
  out <- make_incidence(dates = dates,
                        interval = interval,
                        groups = groups,
                        na_as_group = na_as_group,
                        first_date = first_date,
                        last_date = last_date,
                        ...)
  out$dates    <- as.integer(out$dates)
  out$timespan <- as.integer(out$timespan)
  out$interval <- as.integer(out$interval)
  out
}

#' @export
#' @rdname incidence

incidence.numeric <- function(dates, interval = 1L, groups = NULL,
                              na_as_group = TRUE, first_date = NULL,
                              last_date = NULL, ...) {
  dots <- check_dots(list(...), names(formals(incidence.numeric)))
  interval  <- valid_interval_integer(interval)
  ## make sure input can be used
  out <- make_incidence(dates = dates,
                        interval = interval,
                        groups = groups,
                        na_as_group = na_as_group,
                        first_date = first_date,
                        last_date = last_date,
                        ...)
  out$dates <- as.numeric(out$dates)
  out
}

#' @export
#' @rdname incidence

incidence.POSIXt <- function(dates, interval = 1L, standard = TRUE, groups = NULL,
                             na_as_group = TRUE, first_date = NULL,
                             last_date = NULL, ...) {
  ## make sure input can be used

  dots <- check_dots(list(...), names(formals(incidence.Date)))
  dates <- check_dates(dates)

  ret <- incidence(as.Date(dates),
                   interval = interval,
                   standard = standard,
                   groups = groups,
                   na_as_group = na_as_group,
                   first_date = first_date,
                   last_date = last_date,
                   ...)

  f <- if (inherits(dates, "POSIXct")) as.POSIXct else as.POSIXlt
  ret$dates <- f(ret$dates)
  ret
}

