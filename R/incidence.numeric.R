#' This is the method that is used when the input into incidence() is of the class numeric. This method shifts the values so that the first incidence is on day 1. incidenceComputation is then used to compute the number of incidents per interval. This result is then again shifted back to the original starting date.
#'
#' @param SymptomOnset An nx1 numerical vector, where n is the number of cases
#' @param Interval An integer (number of days in the interval, default = 1)
#' @return A named list with 2 members (Days [first day per interval] and NoOfCases [count of incidents during the respective interval])
#' @examples
#' incidenceComputation(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2))
#' incidenceComputation(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2), 2)
incidence.numeric <- function(SymptomOnset, Interval = 1, ...) {

  # Delete after testing and before creating package
  print("Numeric")

  # Move so that the first incidence is on day 1 (assumed in
  # incidenceComputation)
  SymptomOnsetNumeric <- SymptomOnset - min(SymptomOnset) + 1

  # Use incidenceComputation to compute the incidents
  Incidents <- incidenceComputation(SymptomOnsetNumeric, Interval)

  # Shift data back to the starting value of the input
  Incidents$Days <- Incidents$Days + min(SymptomOnset) - 1
  Incidents
}
