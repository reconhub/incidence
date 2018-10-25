#' Get timespan for an incidence object
#'
#' @inheritParams get_dates
#' @return an `integer` denoting the timespan represented by the incidence
#'   object.
#' @export
#' @examples
#' i <- incidence(sample(-3:50, 100, replace = TRUE))
#' get_timespan(i)
get_timespan <- function(x) {
  UseMethod("get_timespan")
}

#' @export
#' @rdname get_timespan
get_timespan.default <- function(x) {
    stop(sprintf("Not implemented for class %s",
                 paste(class(x), collapse = ", ")))
}

#' @export
#' @rdname get_timespan
get_timespan.incidence <- function(x) {
  x$timespan
}
