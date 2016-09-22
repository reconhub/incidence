##' @export
##'
##' @importFrom graphics plot
##'
##' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
##'
##' @examples
##'
##' if(require(outbreaks)) {
##'   inc <- incidence(ebola.sim$linelist$date.of.onset)
##'   inc
##'   plot(inc)
##' }
##'
plot.incidence <- function(x, ..., xlab = "", ylab = NULL) {
    df <- as.data.frame(x)

    ## Use custom labels for usual time intervals
    if(is.null(ylab)){
        if(x$interval == 1) {
            ylab <- "Daily incidence"
        } else if(x$interval == 7) {
            ylab <- "Weekly incidence"
        } else if(x$interval == 14) {
            ylab <- "Biweekly incidence"
        } else {
            ylab <- sprintf("Incidence by period of %d days", x$interval)
        }
    }

    out <- ggplot2::ggplot(df, ggplot2::aes(x=dates, y=counts)) +
        ggplot2::geom_bar(stat="identity", width=1, color="white") +
            ggplot2::labs(x = xlab, y = ylab)
    out
}

##' @export
plot.incidence_summary <- function(x, ..., ylab = NULL) {
  if (is.null(ylab)) {
    ylab <- sprintf("Cases (%d day %s)", x$interval,
                    if (x$rolling) "rolling average" else "total")
  }
  plot.incidence(x, ..., ylab = ylab)
}
