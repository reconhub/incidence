#' Get n for an incidence object
#'
#' @inheritParams get_dates
#' @return an `integer` denoting the number of samples represented by the 
#'   incidence object.
#' @export
#' @examples
#' i <- incidence(sample(-3:50, 100, replace = TRUE))
#' get_n(i)
get_n <- function(x) {
  UseMethod("get_n")
}

#' @export
#' @rdname get_n
get_n.default <- function(x) {
    stop(sprintf("Not implemented for class %s",
                 paste(class(x), collapse = ", ")))
}

#' @export
#' @rdname get_n
get_n.incidence <- function(x) {
  x$n
}
