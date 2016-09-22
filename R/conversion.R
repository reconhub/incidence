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

##' @export
incidence.Date <- function(onset, ...) {
  ret <- incidence(as.integer(onset), ...)
  ## NOTE: I don't know why as.Date / as.integer aren't transitive but
  ## ?Date reports the 1970-01-01 offset.
  ret$day <- as.Date(ret$day, "1970-01-01")
  ret
}

##' @export
incidence.POSIXt <- function(onset, ...) {
  ret <- incidence(as.Date(onset))
  f <- if (inherits(onset, "POSIXct")) as.POSIXct else as.POSIXlt
  ret$day <- f(ret$day)
  ret
}
