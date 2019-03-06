#' @export
#' @rdname incidence
#' @param x An 'incidence' object.
print.incidence <- function(x, ...) {
  cat("<incidence object>\n")
  cat(sprintf("[%d cases from days %s to %s]\n",
              sum(x$n), min(x$dates), max(x$dates)))
  if ("isoweeks" %in% names(x)) {
    cat(sprintf("[%d cases from ISO weeks %s to %s]\n",
                sum(x$n), head(x$isoweeks, 1), tail(x$isoweeks, 1)))
  }
  if (!is.null(group_names(x))) {
    groups.txt <- paste(group_names(x), collapse = ", ")
    cat(sprintf("[%d groups: %s]\n", ncol(x), groups.txt))
  }
  cat(sprintf("\n$counts: matrix with %d rows and %d columns\n",
              nrow(x$counts), ncol(x$counts)))
  cat(sprintf("$n: %d cases in total\n", x$n))
  cat(sprintf("$dates: %d dates marking the left-side of bins\n",
              length(x$dates)))
  if (is.integer(x$interval)) {
    cat(sprintf("$interval: %d %s\n",
                x$interval, ifelse(x$interval < 2, "day", "days")))
  } else if (grepl("\\d",  x$interval)) {
    cat(sprintf("$interval: %s\n", x$interval))
  } else {
    cat(sprintf("$interval: 1 %s\n", x$interval))
  }
  cat(sprintf("$timespan: %d days\n", x$timespan))
  if (!is.null(x$cumulative)) {
    cat(sprintf("$cumulative: %s\n", x$cumulative))
  }
  cat("\n")
  invisible(x)
}



#' @export
#' @rdname fit
#' @param ... currently unused.
print.incidence_fit <- function(x, ...) {

  cat("<incidence_fit object>\n\n")
  cat("$model: regression of log-incidence over time\n\n")

  cat("$info: list containing the following items:\n")
  cat("  $r (daily growth rate):\n")
  print(x$info$r)
  cat("\n  $r.conf (confidence interval):\n")
  print(x$info$r.conf)
  if (x$info$r[1] > 0) {
    cat("\n  $doubling (doubling time in days):\n")
    print(x$info$doubling)
    cat("\n  $doubling.conf (confidence interval):\n")
    print(x$info$doubling.conf)
  } else {
    cat("\n  $halving (halving time in days):\n")
    print(x$info$halving)
    cat("\n  $halving.conf (confidence interval):\n")
    print(x$info$halving.conf)
  }

  cat(sprintf(
    "\n  $pred: data.frame of incidence predictions (%d rows, %d columns)\n",
    nrow(x$info$pred), ncol(x$info$pred)))
  invisible(x)
}

#' @export
#' @rdname fit
print.incidence_fit_list <- function(x, ...) {
  cat("<list of incidence_fit objects>\n\n")
  cat("attr(x, 'locations'): list of vectors with the locations of each incidence_fit object\n\n")
  locations <- attr(x, "locations")
  cat(sprintf("'%s'", vapply(locations, paste, character(1), collapse = "', '")), sep = "\n")
  cat("\n")
  cat("$model: regression of log-incidence over time\n\n")

  cat("$info: list containing the following items:\n")
  cat("  $r (daily growth rate):\n")
  print(get_info(x, "r"))
  cat("\n  $r.conf (confidence interval):\n")
  print(get_info(x, "r.conf"))
  if (any(get_info(x, "r") > 0)) {
    cat("\n  $doubling (doubling time in days):\n")
    print(get_info(x, "doubling", na.rm = TRUE))
    cat("\n  $doubling.conf (confidence interval):\n")
    print(get_info(x, "doubling.conf", na.rm = TRUE))
  }
  if (any(get_info(x, "r") < 0)) {
    cat("\n  $halving (halving time in days):\n")
    print(get_info(x, "halving", na.rm = TRUE))
    cat("\n  $halving.conf (confidence interval):\n")
    print(get_info(x, "halving.conf", na.rm = TRUE))
  }
  preds <- get_info(x, "pred")
  cat(sprintf(
    "\n  $pred: data.frame of incidence predictions (%d rows, %d columns)\n",
    nrow(preds), ncol(preds)))
  invisible(x)
}

