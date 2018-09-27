#' Estimate the peak date of an incidence curve using bootstrap
#'
#' This function can be used to estimate the peak of an epidemic curve stored as
#' `incidence`, using bootstrap. See [incidence::bootstrap] for more information
#' on the resampling.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}, with inputs on
#'   caveats from Michael Höhle.
#'
#' @md
#'
#' @export
#'
#' @details Input dates are resampled with replacement to form bootstrapped
#'   datasets; the peak is reported for each, resulting in a distribution of
#'   peak times. When there are ties for peak incidence, only the first date is
#'   reported.
#'
#' Note that the bootstrapping approach used for estimating the peak time makes
#' the following assumptions:
#'
#' - the total number of event is known (no uncertainty on total incidence)
#' - dates with no events (zero incidence) will never be in bootstrapped datasets
#' - the reporting is assumed to be constant over time, i.e. every case is
#' equally likely to be reported
#'
#' @param x An `incidence` object.
#'
#' @param n The number of bootstrap datasets to be generated; defaults to 100.
#'
#' @param alpha The type 1 error chosen for the confidence interval; defaults to
#'   0.05.
#'
#' @return A list containing the following items:
#'
#' - `observed`: the peak incidence of the original dataset
#' - `estimated`: the mean peak time of the bootstrap datasets
#' - `ci`: the confidence interval based on bootstrap datasets
#' - `peaks`: the peak times of the bootstrap datasets
#'
#' @seealso [incidence::bootstrap] for the bootstrapping underlying this
#'   approach and [incidence::find_peak] to find the peak in a single
#'   `incidence` object.
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
#'   peak_data <- estimate_peak(i)
#'   peak_data
#'   summary(peak_data$peaks)
#'
#'   ## show confidence interval
#'   plot(i) + geom_vline(xintercept = peak_data$ci, col = "red", lty = 2)
#'
#'   ## show the distribution of bootstrapped peaks
#'   df <- data.frame(peak = peak_data$peaks)
#'   plot(i) + geom_density(data = df,
#'                          aes(x = peak, y = 10 * ..scaled..),
#'                          alpha = .2, fill = "red", color = "red")
#'
#' })}
#'

estimate_peak <- function(x, n = 100, alpha = 0.05) {
  if (!inherits(x, "incidence")) {
    stop("x is not an incidence object")
  }

  if (ncol(x$counts) > 1L) {
    msg <- paste("'x' is stratified by groups",
                 "pooling groups before finding peaks",
                 sep = "\n")
    message(msg)
    x <- pool(x)
  }

  out <- list()

  ## use it to find CI for epidemic peak
  out$observed <- find_peak(x)

  ## peaks on 'n' bootstrap samples
  peak_boot <- replicate(n,
                         find_peak(bootstrap(x)),
                         simplify = FALSE)

  ## convert to vector without losing Date class
  peak_boot <- do.call(c, peak_boot)

  ## store relevant stats and sod off
  out$estimated <- mean(peak_boot)
  QUANTILE <- if(inherits(peak_boot, c("Date", "POSIX"))) quantile_Date else stats::quantile
  out$ci <- QUANTILE(peak_boot, c(alpha / 2, 1 - alpha / 2))
  out$peaks <- peak_boot
  out
}
