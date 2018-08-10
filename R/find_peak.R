#' Find the peak date of an incidence curve
#'
#' This function can be used to find the peak of an epidemic curve stored as an
#' `incidence` object.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}, Zhian N. Kamvar 
#'   \email{zkamvar@@gmail.com}
#'
#' @md
#'
#' @export
#'
#' @param x An `incidence` object.
#' @param pool If `TRUE` (default), any groups will be pooled before finding
#'   a peak. If `FALSE`, separate peaks will be found for each group.
#'
#' @return The date of the (first) highest incidence in the data.
#'
#' @seealso [estimate_peak()] for bootstrap estimates of the peak time
#'
#' @examples
#'
#' if (require(outbreaks) && require(ggplot2)) { withAutoprint({
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
#' })}
#'

find_peak <- function(x, pool = TRUE) {
  if (!inherits(x, "incidence")) {
    stop("x is not an incidence object")
  }

  if (ncol(x$counts) > 1L && pool) {
    msg <- paste("'x' is stratified by groups",
                 "pooling groups before finding peaks",
                 sep = "\n")
    message(msg)
    x <- pool(x)
  }
  the_max <- apply(get_counts(x),
		   MARGIN = 2L,
		   FUN    = which.max
		  )

  out <- stats::setNames(x$dates[the_max], colnames(x$counts))
  out
}
