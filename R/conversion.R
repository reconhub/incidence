##' @importFrom stats as.ts
##' @export
as.ts.incidence <- function(x, ...) {
  stats::ts(x$cases, frequency = 1, start = x$day[[1L]])
}

##' @export
as.ts.incidence_summary <- function(x, ...) {
  if (x$rolling) {
    stop("Can't convert a rolling mean summary to a time series")
  }
  stats::ts(x$cases, frequency = x$interval, start = x$day[[1L]])
}

##' @export
as.data.frame.incidence <- function(x, ...){
    data.frame(dates=x$dates, counts=x$counts)
}
