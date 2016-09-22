##' @export
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
plot.incidence <- function(x, interval = 1L, rolling = FALSE, ...,
                           las = 1, xlab = "Days", ylab = "Cases") {
  ylim <- c(0, max(x$cases))
  plot(x$day, x$cases, type="s", ylim = ylim,
       las = las, xlab = xlab, ylab = ylab)
}

##' @export
plot.incidence_summary <- function(x, ..., ylab=NULL) {
  if (is.null(ylab)) {
    ylab <- sprintf("Cases (%d day %s)", x$interval,
                    if (x$rolling) "rolling average" else "total")
  }
  plot.incidence(x, ..., ylab = ylab)
}
