# This is the method that is used when the input into incidence() is of the
# class Date. This method transforms the input into numeric values and then
# shifts the values so that the first incidence is on day 1.
# incidenceComputation is then used to compute the number of incidents per
# interval. This result is then again shifted back to the original starting
# date, and transformed back into Date.

# Input:
#     SymptomOnset  - an nx1 Date vector, where n is the number of cases
#     Interval      - an integer (number of days in the interval)
# Output:
#     Incidents     - a named list with 2 members (Days [first day per interval]
#                     and NoOfCases [count of incidents during the respective
#                     interval])

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
