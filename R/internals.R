
## These functions are meant for internal use only, and are not
## exported. Functions which check content return it, after potential trivial
## conversions.

#' Count dates within bins
#'
#' @param dates a vector of dates, integers, or numerics
#' @param breaks an ordered vector of dates or integers
#'
#' @author Thibaut Jombart
#' @return an integer vector of the number of incidences per date
#' @noRd
#'
count.dates <- function(dates, breaks){
  counts <- table(cut(as.integer(dates), breaks = c(breaks, Inf), right = FALSE))
  as.integer(counts)
}


## Implement isTRUE and isFALSE to avoid dep on R 3.5.0

isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

isTRUE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}

## A fix for the nonesensical behaviour of `sample` when first argument is of
## length 1.

sample_ <- function(x, ...) {
  x[sample.int(length(x), ...)]
}

## quantiles for Date objects

quantile_Date <- function(x, ...) {
  if (!inherits(x, "Date")) {
    stop("'x' is not a 'Date' object")
  }

  first_date <- min(x, na.rm = TRUE)
  x_num <- as.numeric(x - min(x))
  out <- stats::quantile(x_num, ...)
  first_date + out
}
