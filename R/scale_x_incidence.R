#' @param ... arguments passed to [ggplot2::scale_x_date()],
#' [ggplot2::scale_x_datetime()], or [ggplot2::scale_x_continuous()], depending
#' on how the `$date` element is stored in the incidence object.
#' @export
#' @rdname plot.incidence
scale_x_incidence <- function(x, n_breaks = 6, labels_week = TRUE, ...) {

  breaks <- make_breaks(x, n_breaks, labels_week)


  if (inherits(x$dates, "Date")) {
    
    out <- ggplot2::scale_x_date(breaks = breaks$breaks, 
                                 labels = breaks$labels,
                                 ...)
  } else if (inherits(x$dates, "POSIXt")) {
    breaks$breaks <- as.POSIXct(as.POSIXlt(breaks$breaks))
    out <- ggplot2::scale_x_datetime(breaks   = breaks$breaks,
                                     labels   = breaks$labels,
                                     timezone = "UTC",
                                     ...
                                     )
  } else {
    out <- ggplot2::scale_x_continuous(breaks = breaks$breaks, ...)
  }
  out
}

#' @export
#' @rdname plot.incidence
make_breaks <- function(x, n_breaks = 6L, labels_week = TRUE) {
  stopifnot(inherits(x, "incidence"), is.logical(labels_week), is.numeric(n_breaks))
  ## Defining breaks for the x axis --------------------------------------------
  ##
  ## The x axis can either be integers, Dates, or POSIXt scales. Moreover,
  ## we need to make sure that the breaks align with the left-hand side of the
  ## bins (for now). This section first defines what the breaks should be 
  ## and then treats them according to whether or not the interval was specified
  ## as a character. 
  if (n_breaks == nrow(x)) {
    # The number of breaks are equal to the number of dates... don't worry about
    # adjusting
    breaks <- x$dates  
  } else {
    # adjust breaks to force first date to beginning.
    breaks <- pretty(x$dates, n_breaks)
    breaks <- breaks + (x$dates[1] - breaks[1])
  }
  ## Defining the x axis scale -------------------------------------------------
  ## 
  ## Choosing between scale_x_date, scale_x_datetime, and scale_x_continuous

  # labels should be dates or numbers
  if (is.character(x$interval)) {
    # The interval is a character like "2 weeks" and we have to figure out how
    # to split these manually
    has_number <- grepl("\\d", x$interval)
    tims       <- ceiling(x$timespan/(n_breaks*mean(get_interval(x, integer = TRUE))))
    if (has_number) {
      ni <- as.integer(strsplit(x$interval, " ", fixed = TRUE)[[1L]][1L])
      # the replacement should be a multiple of the number
      replacement <- if (tims <= ni) ni else ceiling(tims/ni)*ni 
      db <- gsub("\\d+", replacement, x$interval)
    } else if (x$interval == "quarter") { 
      db <- paste(tims * 3, "months")
    } else {
      db <- sprintf("%d %s", tims, x$interval)
    }
    breaks <- seq(x$dates[1], x$dates[nrow(x)], by = db)
  }

  if (!is.null(x$weeks)) {
    # If the data are in weeks, we should make sure that the line up correctly
    w <- aweek::date2week(breaks, 
                          week_start = attr(x$weeks, "week_start"), 
                          floor_day = TRUE)
    breaks <- aweek::week2date(w)
    labels <- if (labels_week) w else ggplot2::waiver() 
  } else {
    labels <- ggplot2::waiver()
  } 
  list(breaks = breaks, labels = labels)
}
