#' translate user input to the start date of the week
#' 
#' @param a weekday specification: ISOweek, MMWRweek, EPIweek, Mon-week, Tue-week, etc. 
#'
#' @return the corresponding weekday
#' @keywords internal
#' @noRd
#' @examples
#' get_week_start("ISOweek")
#' get_week_start("MMWRweek")
#' get_week_start("EPIweek")
#'
#' # weeks that start on saturday
#' 
#' get_week_start("Sat-week")
#' get_week_start("week: Saturday")
#' get_week_start("epiweek: Saturday")
#' get_week_start("week: 6")
get_week_start <- function(weekday) {
  wkdy <- gsub('[[:punct:][:blank:]]*', "", tolower(weekday))
  wkdy <- gsub("week", "", wkdy)
  res <- switch(wkdy, 
                "mmwr" = "sunday", # MMWR == CDC epiweek
                "epi"  = "sunday", # CDC epiweek
                "iso"  = "monday", # ISOweek == WHO epiweek
                wkdy # all others
                )
  res <- gsub("epi", "", res) # if they specify something like "epiweek:saturday"
  suppressWarnings(rn <- as.integer(res))
  if (is.na(rn)) res else rn
}
