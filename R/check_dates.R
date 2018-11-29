#' Check date cromulence
#'
#' This function checks that usable dates are provided, and set non-finite
#' values to NA. It also makes a few trivial conversions on the fly.
#' 
#' @param x a vector that represents dates. Can be in almost any format
#' @param error_on_NA a logical specifing whether or not an error should be
#'   thrown if NAs are present in the dates. Defaults to FALSE.
#' @return an object in either integer, Date, or POSIXt
#' @noRd
check_dates <- function(x, error_on_NA = FALSE, ...) {

  if (is.null(x)) {
    stop("dates is NULL", call. = FALSE)
  }

  if (is.character(x)) {
    x <- as.Date(x, ...)
  }

  not_finite <- !is.finite(x)
  if (sum(not_finite) > 0) {
    x[not_finite] <- NA
  }

  if (any(is.na(x)) && error_on_NA) {
    msg <- "NA detected in the dates"
    stop(msg, call. = FALSE)
  }

  if (sum(!is.na(x)) < 1) {
    stop("At least one (non-NA) date must be provided", call. = FALSE)
  }

  if (inherits(x, "Date")) {
    check_timespan(x)
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    check_timespan(x)
    return(x)
  }

  if (is.integer(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    x_ori <- x
    x <- as.integer(floor(x))
    if (!isTRUE(note <- all.equal(x, x_ori))) {
      msg <- paste0(
        "Flooring from non-integer date caused approximations:\n",
        note)
      warning(msg, call. = FALSE)
    }
    return(x)
  }


  formats <- c("Date", "POSIXct", "integer", "numeric", "character")
  msg <- paste0(
    "Input could not be converted to date. Accepted formats are:\n",
    paste(formats, collapse = ", "))
  stop(msg)

}

