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
##' @param long A logical indicating if the output data.frame should be 'long', i.e. where a single
##' column containing 'groups' is added in case of data computed on several groups.
as.data.frame.incidence <- function(x, ..., long = FALSE){
    counts <- x$counts
    if (ncol(counts) == 1L) {
        colnames(counts) <- "counts"
    }

    out <- cbind.data.frame(dates=x$dates, counts)

    ## handle the long format here
    if (long && ncol(x$counts)>1) {
        n.groups <- ncol(out) - 1
        groups <- factor(rep(colnames(x$counts), each = nrow(out)))
        counts <- as.vector(x$counts)
        out <- data.frame(dates = out[1], counts = counts, groups = groups)

    }

    out
}
