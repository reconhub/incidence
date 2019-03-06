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
#' class: integer, numeric, Date, POSIXct, POSIXlt, and character. (See Note 
#' about `numeric` and `character` formats) 
#'
#' @param interval An integer or character indicating the (fixed) size of the
#' time interval used for computing the incidence; defaults to 1 day. This can
#' also be a text string that corresponds to a valid date interval: day, week,
#' month, quarter, or year. (See Note)
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
#' @note \subsection{Input data (`dates`)}{
#'  - **Decimal (numeric) dates**: will be truncated with a warning
#'  - **Character dates** should be in the unambiguous `yyyy-mm-dd` (ISO 8601)
#'   format. Any other format will trigger an error.
#' }
#' 
#' \subsection{Interval specification (`interval`)}{
#' If `interval` is a valid character (e.g. "week" or "month"), then
#' the bin will start at the beginning of the interval just before the first
#' observation by default. For example, if the first case was recorded on
#' Wednesday, 2018-05-09:
#'
#'  - "week"    : first day of the ISOweek (i.e. Monday, 2018-05-07)
#'  - "month"   : first day of the month (i.e. 2018-05-01)
#'  - "quarter" : first day of the quarter (i.e. 2018-04-01)
#'  - "year"    : first day of the calendar year (i.e. 2018-01-01)
#'
#' These default intervals can be overridden with `standard = FALSE`, which
#' sets the interval to begin at the first observed case.
#'
#' \subsection{The `first_date` argument}{
#' Previous versions of _incidence_ had the `first_date` argument override
#' `standard = TRUE`. It has been changed as of _incidence_ version 1.6.0 to
#' be more consistent with the behavior when `first_date = NULL`. This, however
#' may be a change in behaviour, so a warning is now issued once and only once
#' if `first_date` is specified, but `standard` is not. To never see this
#' warning, use `options(incidence.warn.first_date = FALSE)`. 
#' }
#'
#' The intervals for "month", "quarter", and "year" will necessarily vary in the
#' number of days they encompass and warnings will be generated when the first
#' date falls outside of a calendar date that is easily represented across the
#' interval.
#' }
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
#'   onset <- outbreaks::ebola_sim$linelist$date_of_onset
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
#' # Use of first_date
#' d <- Sys.Date() + sample(-3:10, 10, replace = TRUE)
#' 
#' # `standard` specified, no warning
#' di <- incidence(d, interval = "week", first_date = Sys.Date() - 10, standard = TRUE)
#'
#' # warning issued if `standard` not specified
#' di <- incidence(d, interval = "week", first_date = Sys.Date() - 10)
#'
#' # second instance: no warning issued
#' di <- incidence(d, interval = "week", first_date = Sys.Date() - 10)
#'
#'
incidence <- function(dates, interval = 1L, ...) {
  UseMethod("incidence")
}


#' @export
#' @rdname incidence
incidence.default <- function(dates, interval = 1L, ...) {
  incidence(check_dates(dates), interval = interval, ...)
}

#' @export
#' @rdname incidence
#'
#' @param standard (Only applicable to Date objects) When `TRUE` (default) and the
#'   `interval` one of "week", "month", "quarter", or "year", then this will
#'   cause the bins for the counts to start at the beginning of the interval
#'   (See Note). 

incidence.Date <- function(dates, interval = 1L, standard = TRUE, groups = NULL,
                           na_as_group = TRUE, first_date = NULL,
                           last_date = NULL, ...) {
  the_call <- match.call()
  warnme <- getOption('incidence.warn.first_date', FALSE) 
  if (warnme && !is.null(the_call[["first_date"]]) && is.null(the_call[["standard"]])) {
    fd  <- as.character(deparse(the_call[["first_date"]]))
    msg <- "\n\nAs of incidence version 1.6.0, the default behavior has been"
    msg <- paste(msg, "modified so that `first_date` no longer overrides")
    msg <- paste(msg, "`standard`. If you want to use %s as the precise")
    msg <- paste(msg, "`first_date`, set `standard = FALSE`.")
    msg <- paste(msg, "To remove this warning in the future,  explicitly set the `standard` argument OR use `options(incidence.warn.first_date = FALSE)`\n", sep = "\n\n")
    warning(sprintf(msg, fd))
    # turn the warning off so that it's not so noisy
    options(incidence.warn.first_date = FALSE)
  }
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
    week_start   <- get_week_start(interval)
    out$isoweeks <- as.character(aweek::date2week(out$dates, week_start, floor_day = TRUE))
  }

  out
}


#' @export
#' @rdname incidence
incidence.character <- function(dates, interval = 1L, standard = TRUE, groups = NULL,
                           na_as_group = TRUE, first_date = NULL,
                           last_date = NULL, ...) {
  iso_std <- grepl("^[0-9]{4}-[01][0-9]-[0-3][0-9]$", trimws(dates))
  iso_std[is.na(dates)] <- TRUE # prevent false alarms
  if (!all(iso_std)) {
    msg <- paste("Not all dates are in ISO 8601 standard format (yyyy-mm-dd).",
                 "The first incorrect date is %s"
    )
    stop(sprintf(msg, dates[!iso_std][1]))
  }
  dots  <- check_dots(list(...), names(formals(incidence.Date)))
  dates <- check_dates(dates)

  ret <- incidence(as.Date(trimws(dates)),
                   interval = interval,
                   standard = standard,
                   groups = groups,
                   na_as_group = na_as_group,
                   first_date = first_date,
                   last_date = last_date,
                   ...)
  ret
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

  dots  <- check_dots(list(...), names(formals(incidence.Date)))
  dates <- check_dates(dates)

  ret <- incidence(as.Date(dates),
                   interval = interval,
                   standard = standard,
                   groups = groups,
                   na_as_group = na_as_group,
                   first_date = first_date,
                   last_date = last_date,
                   ...)

  ret$dates <- as.POSIXlt(ret$dates)
  if (inherits(dates, "POSIXct")) {
    ret$dates <- as.POSIXct(ret$dates)
  }
  ret
}

