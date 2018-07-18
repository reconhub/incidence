#' Retrieve dates from an incidence object
#'
#' @param x an [incidence] object
#' @param ... Unused
#'
#' @return a vector of dates or numerics
#' @export
#'
#' @examples
#'
#' set.seed(999)
#' dat <- as.Date(Sys.Date()) + sample(-3:50, 100, replace = TRUE)
#' x <- incidence(dat, interval = "month")
#' get_dates(x)
#' get_dates(x, position = "middle")
get_dates <- function(x, ...) {
  UseMethod("get_dates")
}


#' @param position One of "left", "center", "middle", or "right" specifying what
#'   side of the bin the date should be drawn from.
#' @param count_days If `TRUE`, the result will be represented as the number of
#'   days from the first date.
#'
#' @rdname get_dates
#' @keywords accessors
#' @export
#'
#' @examples
#' set.seed(999)
#' dat <- as.Date(Sys.Date()) + sample(-3:50, 100, replace = TRUE)
#' x <- incidence(dat, interval = "month")
#' get_dates(x)
#' get_dates(x, "center")
#' get_dates(x, "right")
#'
#' # Return dates by number of days from the first date
#' get_dates(x, count_days = TRUE)
#' get_dates(incidence(-5:5), count_days = TRUE)
get_dates.incidence <- function(x, position = "left", count_days = FALSE, ...) {
  POSITION <- match.arg(position, c("left", "center", "middle", "right"))
  if (!count_days && POSITION == "left") return(x$dates)
  # Default: left side of bins
  first_date <- min(x$dates)
  res        <- x$dates - first_date
  if (POSITION %in% c("center", "middle", "right")) {
    divisor <- if (POSITION == "right") 1L else 2L
    res     <- res + get_interval(x, integer = TRUE)/divisor
  }
  # This part is necessary to avoid the Date class rounding the result -_-
  res <- as.numeric(res)
  if (count_days) {
    return(res)
  } else {
    return(first_date + res)
  }
}
