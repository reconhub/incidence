#' Get the interval for an incidence object.
#'
#' @param x an [incidence] object.
#' @param ... Unused
#'
#' @return if `integer = TRUE`: an integer vector, otherwise: the value stored
#'   in `x$interval`
#' @export
#' @keywords accessors
#'
#' @examples
#'
#' set.seed(999)
#' dat <- as.Date(Sys.Date()) + sample(-3:50, 100, replace = TRUE)
#' x <- incidence(dat, interval = "month")
#' get_interval(x)
#' get_interval(x, integer = FALSE)
get_interval <- function(x, ...) {
  UseMethod("get_interval")
}

#' @rdname get_interval
#' @param integer When `TRUE` (default), the interval will be converted to an
#'   integer vector if it is stored as a character in the incidence object.
#' @export
get_interval.incidence <- function(x, integer = TRUE, ...) {
  if (!integer || is.numeric(x$interval)) {
    return(x$interval)
  }
  if (is.character(x$interval)) {
    res <- switch(x$interval,
                  day     = 1L,
                  week    = 7L,
                  month   = get_days_in_month(x$dates),
                  quarter = get_days_in_quarter(x$dates),
                  year    = get_days_in_year(x$dates)
                 )
    return(res)
  } else {
    stop(sprintf("I don't know how to convert a %s to an integer",
                 paste(class(x$interval), collapse = ", ")))
  }
}
