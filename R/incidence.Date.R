#' Method for input of class Date.
#'
#' This is the method that is used when the input into incidence() is of the
#' class Date.
#'
#' This method transforms the input into numeric values and then shifts the
#' values so that the first incidence is on day 1. incidenceComputation is then
#' used to compute the number of incidents per interval. This result is then
#' again shifted back to the original starting date, and transformed back into
#' Date.
#' @param SymptomOnset An nx1 vector of class Date, where n is the number of
#'   cases
#' @param Interval An integer (number of days in the interval, default = 1)
#' @return A named list with 2 members (Days [first day per interval] and
#'   NoOfCases [count of incidents during the respective interval])
#' @examples
#' incidence(c("2016-01-01", "2016-01-05", "2016-01-08", "2016-01-03", "2016-01-07", "2016-01-02", "2016-01-04", "2016-01-06", "2016-01-09", "2016-01-02"))
#' incidence(c("2016-01-01", "2016-01-05", "2016-01-08", "2016-01-03", "2016-01-07", "2016-01-02", "2016-01-04", "2016-01-06", "2016-01-09", "2016-01-02"), 2)
incidence.Date <- function(SymptomOnset, Interval = 1, ...) {

  # Delete after testing and before creating package
  print("Date")

  # Dates are represented as the number of days since 1970-01-01
  # Transform SymptomOnset to numeric
  SymptomOnsetNumericTemp <- as.numeric(SymptomOnset)

  # Move so that the first incidence is on day 1
  SymptomOnsetNumeric <- SymptomOnsetNumericTemp - min(SymptomOnsetNumericTemp) + 1

  # Use incidenceComputation to compute the incidents
  Incidents <- incidenceComputation(SymptomOnsetNumeric, Interval)

  # Move data back to the original starting input value and original class
  Incidents$NoOfCases <- Incidents$NoOfCases
  Incidents$Days      <- as.Date(Incidents$Days + min(SymptomOnsetNumericTemp) - 1, origin = "1970-01-01")
  Incidents
}
