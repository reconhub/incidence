#' @return 
#'   - `get_n()` The total number of cases stored in the object
#' @export
#' @rdname accessors
#' @aliases get_n
get_n <- function(x) {
  UseMethod("get_n")
}

#' @export
#' @rdname accessors
#' @aliases get_n.default
get_n.default <- function(x) {
    stop(sprintf("Not implemented for class %s",
                 paste(class(x), collapse = ", ")))
}

#' @export
#' @rdname accessors
#' @aliases get_n.incidence
get_n.incidence <- function(x) {
  x$n
}
