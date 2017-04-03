
## These functions are meant for internal use only, and are not
## exported. Functions which check content return it, after potential trivial
## conversions.






## This function checks that usable dates are provided, and set non-finite
## values to NA. It also makes a few trivial conversions on the fly.

check_dates <- function(x, error_on_NA = FALSE, ...) {
  if (is.null(x)) {
    stop("dates is NULL")
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
    stop(msg)
  }

  if (sum(!is.na(x)) < 1) {
    stop("At least one (non-NA) date must be provided")
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
    x <- as.integer(floor(x))
    if (!isTRUE(note <- all.equal(x, x_ori))) {
      msg <- paste0(
        "Flooring from non-integer date caused approximations:\n",
        note)
      warning(msg)
    }
    return(x)
  }


  formats <- c("Date", "POSIXct", "integer", "numeric", "character")
  msg <- paste0(
    "Input could not be converted to date. Accepted formats are:\n",
    paste(formats, collapse = ", "))
  stop(msg)

}






## Non-exported function, enforces that an interval is:
## - strictly positive
## - integer (rounded)
## - finite
## - of length 1
check_interval <- function(x){
  if (missing(x) || is.null(x)) {
    stop("Interval is missing or NULL")
  }
  if (length(x) != 1L) {
    stop(sprintf(
      "Exactly one value should be provided as interval (%d provided)",
      length(x)))
  }
  if (!is.finite(x)) {
    stop("Interval is not finite")
  }
  x <- as.integer(round(old <- x))
  if (x < 1L) {
    stop(sprintf(
      "Interval must be at least 1 (input: %.3f; after rounding: %d)",
      old, x))
  }
  x
}





## Non-exported function, enforces that 'groups' is either NULL or:
## - a factor
## - of the same length as 'dates'
##
## It also treats missing groups (NA) as a separate group is needed.

check_groups <- function(x, dates, na_as_group){
  if (is.null(x)) {
    return(NULL)
  }
  if (na_as_group) {
    x <- as.character(x)
    x[is.na(x)] <- "NA"
  }
  if (length(x) != length(dates)) {
    stop(sprintf(
      "'x' does not have the same length as dates (%d vs %d)",
      length(x), length(dates)))
  }
  factor(x)
}
