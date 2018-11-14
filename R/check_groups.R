#' Enforce cromulence of groups
#'
#' This enforces that groups is either:
#'
#'   - NULL
#'   - a factor and the same length as dates
#' It also treats missing groups (NA) as a separate group is needed.
#' 
#' @param x a vector denoting groups
#' @param dates a vector representing dates
#' @param na_as_group a logical indicating whether or not NA should be
#'   considered a separate group.
#' @noRd
check_groups <- function(x, dates, na_as_group){
  if (is.null(x)) {
    return(NULL)
  }
  x   <- factor(x)
  lev <- levels(x)
  if (na_as_group && any(is.na(x))) {
    x <- as.character(x)
    x[is.na(x)] <- "NA"
    lev <- c(lev, "NA")
  }
  if (length(x) != length(dates)) {
    stop(sprintf(
                 "'x' does not have the same length as dates (%d vs %d)",
                 length(x),
                 length(dates)
                 )
        )
  }
  factor(x, levels = lev)
}
