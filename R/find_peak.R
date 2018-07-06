#' Find the peak date of an incidence curve
#'
#' This function can be used to find the peak of an epidemic curve stored as an
#' `incidence` object.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}.
#'
#' @md
#'
#' @export
#'
#' @param x An `incidence` object.
#'

#' @return The date of the (first) highest incidence in the data.
#'
#' @seealso [incidence::estimate_peak] for bootstrap estimates of the peak time
#'
#' @examples
#'
#' if (require(outbreaks) && require(ggplot2)) {
#'   i <- incidence(fluH7N9_china_2013$date_of_onset)
#'   i
#'   plot(i)
#'
#'   ## one simple bootstrap
#'   x <- bootstrap(i)
#'   x
#'   plot(x)
#'
#'   ## find 95% CI for peak time using bootstrap
#'   find_peak(i)
#'
#'
#'   ## show confidence interval
#'   plot(i) + geom_vline(xintercept = find_peak(i), col = "red", lty = 2)
#'
#' }
#'

find_peak <- function(x) {
  if (!inherits(x, "incidence")) {
    stop("x is not an incidence object")
  }

  if (ncol(x$counts) > 1L) {
    msg <- paste("'x' is stratified by groups",
                 "pooling groups before finding peaks",
                 sep = "\n")
    warning(msg)
    x <- pool(x)
  }
  out <- x$dates[which.max(pool(x)$counts)]
  out
}
