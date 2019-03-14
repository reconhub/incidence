#' Access various elements of an incidence object
#'
#' @param x an [incidence] object.
#' @param ... Unused
#'
#' @return 
#'   - `get_interval()` if `integer = TRUE`: an integer vector, otherwise: the 
#'     value stored in `x$interval`
#' @export
#' @keywords accessors
#'
#' @rdname accessors
#' @aliases get_interval
#' @seealso 
#'   - [get_counts()] to access the matrix of counts
#'   - [get_dates()] to access the dates on the right, left, and center of the
#'       interval.
#'   - [group_names()] to access and possibly re-name the groups
#' @examples
#'
#' set.seed(999)
#' dat <- as.Date(Sys.Date()) + sample(-3:50, 100, replace = TRUE)
#' x <- incidence(dat, interval = "month")
#' 
#' # the value stored in the interval element
#' get_interval(x)
#'
#' # the numeric value of the interval in days
#' get_interval(x, integer = FALSE)
#' 
#' # the number of observations in the object
#' get_n(x)
#' 
#' # the length of time represented
#' get_timespan(x)
#'
#' # the number of groups
#' ncol(x)
#' 
#' # the number of bins (intervals)
#' nrow(x)
get_interval <- function(x, ...) {
  UseMethod("get_interval")
}

#' @export
#' @rdname accessors
#' @aliases get_interval.default
get_interval.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @param integer When `TRUE` (default), the interval will be converted to an
#'   integer vector if it is stored as a character in the incidence object.
#' @export
#' @rdname accessors
#' @aliases get_interval.incidence
get_interval.incidence <- function(x, integer = TRUE, ...) {
  if (!integer || is.numeric(x$interval)) {
    return(x$interval)
  }
  if (is.character(x$interval)) {
    res <- get_interval_type(x$interval)
    n   <- get_interval_number(x$interval)
    res <- switch(res,
                  day     = 1L * n,
                  week    = 7L * n,
                  month   = get_days_in_month(x$dates, n),
                  quarter = get_days_in_quarter(x$dates, n),
                  year    = get_days_in_year(x$dates, n)
                 )
    return(res)
  } else {
    stop(sprintf("I don't know how to convert a %s to an integer",
                 paste(class(x$interval), collapse = ", ")))
  }
}

get_interval_type <- function(x) {
  res <- NULL
  res <- if (grepl("day", x, ignore.case = TRUE)) "day" else res
  res <- if (grepl("week", x, ignore.case = TRUE)) "week" else res
  res <- if (grepl("month", x, ignore.case = TRUE)) "month" else res
  res <- if (grepl("quarter", x, ignore.case = TRUE)) "quarter" else res
  res <- if (grepl("year", x, ignore.case = TRUE)) "year" else res
  res
}

get_interval_number <- function(x) {

  if (!grepl("^\\d", x)) return(1L)
  as.integer(gsub("^(\\d*).+$", "\\1", x))

}


get_days_in_month <- function(dates, m = 1L) {
  dates <- floor_month(dates)
  res <- vapply(strsplit(format(dates), "-"), 
                add_months, 
                character(1),
                months = m)
  as.integer(as.Date(res) - dates)
}

get_days_in_quarter <- function(dates, m = 1L) {
  dates <- floor_month(dates)
  res <- vapply(strsplit(format(dates), "-"),
                FUN = add_months,
                FUN.VALUE = character(1),
                months = 3L * m)
  as.integer(as.Date(res) - dates)
}

get_days_in_year <- function(dates, m = 1L) {
  dates <- floor_month(dates)
  res <- vapply(strsplit(format(dates), "-"),
                FUN = add_months,
                FUN.VALUE = character(1),
                months = 12L * m)
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
