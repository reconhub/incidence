#' Trim observations based on the first and last dates
#'
#' @param observations a vector of dates or integers
#' @param first_date a single date or integer
#' @param last_date a single date or integer
#'
#' @return the trimmed observations as a logical vector
#' @noRd
#' @keywords internal
trim_observations <- function(dates, first_date = NULL, last_date = NULL) {
  # Remove the missing observations -------------------- 
  res <- !is.na(dates)
  if (sum(res) < length(dates)) {
    message(sprintf("%d missing observations were removed.",
                    length(dates) - sum(res)
                    )
    )
  }
  dates <- dates[res]
  # Trim ends ------------------------------------------ 
  res   <- dates >= first_date & dates <= last_date
  if (sum(res) < length(dates)) {
    message(sprintf("%d observations outside of [%s, %s] were removed.",
                    length(dates) - sum(res),
                    format(first_date),
                    format(last_date)
                    )
            )
  }
  res
}
