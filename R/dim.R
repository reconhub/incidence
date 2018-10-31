#' @rdname accessors
#' @export
#' @return
#'    - `dim()` the dimensions in the number of bins and number of groups
dim.incidence <- function(x) {
  dim(x$counts)
}
