##' @export
summary.incidence <- function(object, interval = 1L, rolling = FALSE, ...) {
  if (interval == 1L) {
    return(object)
  }

  if (interval > 1L) {
    if (rolling) {
      cases <- rolling_sum(object$cases, interval)
      day <- object$day[seq_along(cases)]
    } else {
      i <- (seq_along(object$day) - 1L) %/% interval + 1L
      cases <- unname(tapply(object$cases, i, sum))
      day <- object$day[seq(1, along.with=cases, by=interval)]
    }
  }

  ret <- list(day = day, cases = cases,
              interval = interval, rolling = rolling,
              data = object)
  class(ret) <- "incidence_summary"
  ret
}

##' @export
summary.incidence_sumamry <- function(object, ...) {
  summary(object$data, ...)
}

##' @export
print.incidence_summary <- function(x, ...) {
  cat("<summarised incidence object>\n")
  cat(sprintf("  - %d cases\n", sum(x$data$cases)))
  r <- range(x$data$day)
  if (as.integer(r[[1L]]) == 1L) {
    cat(sprintf("  - Spanning %d days\n", diff(r)))
  } else {
    cat(sprintf("  - Spanning %d days [%s - %s]\n", diff(r), r[[1L]], r[[2L]]))
  }
  cat(sprintf("  - Grouped by %d days\n", x$interval))
  cat(sprintf("  - By %s\n", if (x$rolling) "rolling mean" else "binning"))
  invisible(x)
}
