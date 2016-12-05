##' Conversion of indence objects
##'
##' These functions convert \code{incidence} objects into other classes.
##'
##' @rdname conversions
##'
##' @author Thibaut Jombart \email{thibautjombart@@gmail.com}, Rich Fitzjohn
##'
##' @importFrom stats as.ts
##' @export
##'
##' @param x An \code{incidence} object.
##'
##' @param ... Further arguments passed to other functions (no used).
##'
##' @export
##'
##' @seealso the \code{\link{incidence}} function to generate the 'incidence' objects.
##'
##' @param long A logical indicating if the output data.frame should be 'long', i.e. where a single
##' column containing 'groups' is added in case of data computed on several groups.
##'
##' @examples
##' ## create fake data
##' data <- c(0,1,1,2,1,3,4,5,5,5,5,4,4,26,6,7,9)
##' sex <- sample(c("m","f"), length(data), replace=TRUE)
##'
##' ## get incidence per group (sex)
##' i <- incidence(data, groups = sex)
##' i
##' plot(i)
##'
##' ## convert to data.frame
##' as.data.frame(i)
##'
##' ## same, 'long format'
##' as.data.frame(i, long = TRUE)
##'
as.data.frame.incidence <- function(x, ..., long = FALSE){
    counts <- x$counts
    if (ncol(counts) == 1L) {
        colnames(counts) <- "counts"
    }

    if ("isoweeks" %in% names(x)) {
      out <- cbind.data.frame(dates = x$dates, isoweeks = x$isoweeks, counts)
    } else {
      out <- cbind.data.frame(dates = x$dates, counts)
    }

    ## handle the long format here
    if (long && ncol(x$counts) > 1) {
        groups <- factor(rep(colnames(x$counts), each = nrow(out)))
        counts <- as.vector(x$counts)
        if ("isoweeks" %in% names(x)) {
          out <- data.frame(dates = out$dates, isoweeks = out$isoweeks, counts = counts, groups = groups)
        } else {
          out <- data.frame(dates = out$dates, counts = counts, groups = groups)
        }
    }
    out
}
