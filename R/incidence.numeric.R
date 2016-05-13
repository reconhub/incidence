# This is the method that is used when the input into incidence() is of the
# class numeric. This method shifts the values so that the first incidence is on
# day 1. incidenceComputation is then used to compute the number of incidents per
# interval. This result is then again shifted back to the original starting date.

# Input:
#     SymptomOnset  - an nx1 numeric vector, where n is the number of cases
#     Interval      - an integer (number of days in the interval)
# Output:
#     Incidents     - a named list with 2 members (Days [first day per interval]
#                     and NoOfCases [count of incidents during the respective
#                     interval])

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
