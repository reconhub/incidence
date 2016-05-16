#' This is the generic function that decides which method to call, depending on the
#' class of the input object. The possible methods are:
#' incidence.numeric
#' incidence.integer
#' incidence.PoisXlt
#' incidence.PoisXct
#' incidence.Date
#' incidence.list
#' incidence.matrix
#' 
#' @param SymptomOnset An nx1 vector of one of the classes mentioned above, where n
#'    is the number of cases
#' @param Interval An integer (number of days in the interval, default = 1)
#' @return Incidents A named list with 2 members (Days [first day per interval] and
#'    NoOfCases [count of incidents during the respective interval])
#' @examples
#' incidence(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2))
#' incidence(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2), 2)
#' incidence(c("2016-01-01", "2016-01-05", "2016-01-08", "2016-01-03",
#'    "2016-01-07", "2016-01-02", "2016-01-04", "2016-01-06", "2016-01-09",
#'    "2016-01-02"), 2)
incidence <- function(SymptomOnset, Interval = 1, ...) {
  UseMethod("incidence")
}
