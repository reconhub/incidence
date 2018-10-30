#' @return 
#'   - `get_timespan()`: an `integer` denoting the timespan represented by the 
#'   incidence object.
#' @export
#' @rdname accessors
#' @aliases get_timespan
get_timespan <- function(x) {
  UseMethod("get_timespan")
}

#' @export
#' @rdname accessors
#' @aliases get_timespan.default
get_timespan.default <- function(x) {
    stop(sprintf("Not implemented for class %s",
                 paste(class(x), collapse = ", ")))
}

#' @export
#' @rdname accessors
#' @aliases get_timespan.incidence
get_timespan.incidence <- function(x) {
  x$timespan
}
