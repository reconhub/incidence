## Non-exported function extracting info and predictions from a lm object
## - reg is a lm object
## - origin is the first date of the incidence
## - level is a confidence level, defaulting to .95

extract_info <- function(reg, origin, level){
  if (is.null(reg)) {
    return(NULL)
  }

  ## extract growth rates (r)
  ## here we need to keep all coefficients when there are interactions
  to.keep <- grep("^dates.x.*$", names(stats::coef(reg)), value = TRUE)
  r <- stats::coef(reg)[to.keep]
  use.groups <- length(r) > 1
  if (use.groups) {
    if (all(r >= 0) | all(r <= 0)) {
      # continue
    } else {
      stop("Growth rates of groups have different signs; fit groups separately.")
    }
    names(r) <- reg$xlevels[[1]] # names = levels if groups
  } else {
    names(r) <- NULL # no names otherwise
  }
  r.conf <- stats::confint(reg, to.keep, level)
  rownames(r.conf) <- names(r)
  if (use.groups) {
    r[-1] <- r[-1] + r[1] # add coefs to intercept
    r.conf[-1,] <- r.conf[-1,] + r.conf[1,] # add coefs to intercept
  }


  ## need to pass new data spanning all dates and groups here
  if (use.groups) {
    new.data <- expand.grid(sort(unique(reg$model$dates.x)),
                            levels(reg$model$groups))
    names(new.data) <- c("dates.x", "groups")
  } else {
    new.data <- data.frame(dates.x = sort(unique(reg$model$dates.x)))
  }
  pred <- exp(stats::predict(reg, newdata = new.data, interval = "confidence",
                             level = level))
  ## keep track of dates and groups for plotting
  pred <- cbind.data.frame(new.data, pred)
  info <- list(r = r, r.conf = r.conf,
               pred = pred)

  if (r[1] > 0 ) { # note: choice of doubling vs halving only based on 1st group
    info$doubling <- log(2) / r
    info$doubling.conf <- log(2) / r.conf
    o.names <- colnames(info$doubling.conf)
    info$doubling.conf <- info$doubling.conf[, rev(seq_along(o.names)),
                                             drop = FALSE]
    info$doubling.conf[info$doubling.conf < 0] <- Inf
    colnames(info$doubling.conf) <- o.names
  } else {
    info$halving <- log(0.5) / r
    info$halving.conf <- log(0.5) / r.conf
    info$halving.conf[info$halving.conf < 0] <- Inf
  }

  ## We need to store the date corresponding to 'day 0', as this will be used
  ## to create actual dates afterwards (as opposed to mere numbers of days).
  ## origin <- min(x$dates)

  ## Dates are reconstructed from info$pred$dates.x and origin).  Note that
  ## this is approximate, as dates are forced to be integers. A better option
  ## would be to convert the dates to numbers, but ggplot2 is no longer
  ## consistent when mixing up Date and decimal numbers (it works only in some
  ## cases / geom).
  dates <- origin + pred$dates.x
  info$pred <- cbind.data.frame(dates, info$pred)
  out <- list(model = reg, info = info, origin = origin)
  class(out) <- "incidence_fit"
  out
}
