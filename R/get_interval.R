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

get_days_in_month <- function(dates) {
  dates <- floor_month(dates)
  res <- vapply(strsplit(format(dates), "-"), add_months, character(1))
  as.integer(as.Date(res) - dates)
}

get_days_in_quarter <- function(dates) {
  dates <- floor_month(dates)
  res <- vapply(strsplit(format(dates), "-"),
                FUN = add_months,
                FUN.VALUE = character(1),
                months = 3L)
  as.integer(as.Date(res) - dates)
}

get_days_in_year <- function(dates) {
  dates <- floor_month(dates)
  res <- vapply(strsplit(format(dates), "-"),
                FUN = add_months,
                FUN.VALUE = character(1),
                months = 12L)
  as.integer(as.Date(res) - dates)
}

floor_month <- function(x) {
  x - as.integer(format(x, "%d")) + 1L
}

add_months <- function(x, months = 1L) {
  i <- as.integer(x[2]) + months
  if (i > 12L) {
    x[1] <- as.character(as.integer(x[1]) + 1L)
    i    <- i - 12L
  }
  x[2] <- sprintf("%02d", i)
  paste(x, collapse = "-")
}
