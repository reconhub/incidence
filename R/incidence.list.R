#' This is the method that is used when the input into incidence() is of the
#' class list. This method prints a warning if the list has more than one
#' members. It then takes the first member and re-runs incidence, which will choose
#' a method according to the class of the first member of the input.
#'
#' @param SymptomOnset A list with the first member of length n, where n is the
#'    number of cases
#' @param Interval An integer (number of days in the interval, default = 1)
#' @return Incidents A named list with 2 members (Days [first day per interval] and
#'    NoOfCases [count of incidents during the respective interval])
#' @examples
#' incidence(list(c("2016-01-01", "2016-01-05", "2016-01-08", "2016-01-03",
#'    "2016-01-07", "2016-01-02", "2016-01-04", "2016-01-06", "2016-01-09",
#'    "2016-01-02")))
#' incidenceComputation(list(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2)), 2)
incidence.list <- function(SymptomOnset, Interval = 1, ...) {

  # Delete after testing and before creating package
  print("List")

  # Print a warning if there are more than one member of the list
  if (length(SymptomOnset) > 1) {
    warning("The list SymptomOnset has more than one member. Only the first member of the list has been used")  }

  # Choose only the first member of the list
  SymptomOnset_NonList <- SymptomOnset[[1]]

  # Run incidence again
  incidence(SymptomOnset_NonList, Interval)
  }
