
## These functions are meant for internal use only, and are not
## exported. Functions which check content return it, after potential trivial
## conversions.





## Check dates

check_dates <- function(x, ...) {
  if (is.character(x)) {
    x <- as.Date(x, ....)
  }

  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(x)
  }

  if (is.integer(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    x_ori <- x
    x <- as.integer(round(x))
    if (!isTRUE(note <- all.equal(x, x.ori))) {
      msg <- paste0(
        "Conversion from non-integer date caused approximations:",
        note)
      warning(msg)
    }
  }


  formats <- c("Date", "POSIXct", "integer", "numeric")
  msg <- paste0(
    "Input could not be converted to date. Accepted formats are:\n",
    paste(formats, collapse = ", "))
  stop(msg)

}
