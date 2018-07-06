#' Bootstrap incidence time series
#'
#' This function can be used to bootstrap `incidence` objects. Bootstrapping is
#' done by sampling with replacement the original input dates. See `details` for
#' more information on how this is implemented.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @md
#'
#' @export
#'
#' @details As original data are not stored in `incidence` objects, the
#'   bootstrapping is achieved by multinomial sampling of date bins weighted by
#'   their relative incidence.
#'
#' @param x An `incidence` object.
#'
#' @param randomise_groups A `logical` indicating whether groups should be
#'   randomised as well in the resampling procedure; respective group sizes will
#'   be preserved, but this can be used to remove any group-specific temporal
#'   dynamics. If `FALSE` (default), data are resampled within groups.
#'
#' @return An `incidence` object.
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
#'   ## use it to find CI for epidemic peak
#'   find_peak <- function(x) x$dates[which.max(pool(x)$counts)]
#'   find_peak(i)
#'
#'   ## peaks on 100 bootstrap samples
#'   peak_boot <- replicate(100,
#'                          find_peak(bootstrap(i)),
#'                          simplify = FALSE)
#'
#'   ## convert to vector without losing Date class
#'   peak_boot <- do.call(c, peak_boot)
#'
#'   ## check distribution of peaking times
#'   summary(peak_boot)
#'   plot(i) + geom_density(data = data.frame(peak = peak_boot),
#'                          aes(x = peak, y = 10 * ..scaled..),
#'                          alpha = .2, fill = "red", color = "red")
#'
#' }
#'

bootstrap <- function(x, randomise_groups = FALSE) {
  if (!inherits(x, "incidence")) {
    stop("x is not an incidence object")
  }

  ## `counts` is a vector of event counts, meant to be a column of x$counts
  boot_one_group <- function(counts) {
    sample_(x$dates, size = sum(counts), replace = TRUE, prob = counts)
  }

  new_dates <- do.call(c,
                       lapply(1:ncol(x$counts),
                              function(i) boot_one_group(x$counts[, i])))
  group_sizes <- colSums(x$counts)
  new_groups <- rep(colnames(x$counts), group_sizes)

  if (randomise_groups) {
    new_groups <- sample_(new_groups)
  }

  incidence(new_dates, interval = x$interval, groups = new_groups)

}
