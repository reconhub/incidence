# This is the method that is used when the input into incidence() is of the class
# POSIXlt. This method transforms the input into numeric values and divides that
# value by 24*60*60 because as.numeric() of a POSIXlt value gives the number of
# seconds since 1970-01-01. The values are then shifted so that the first incidence
# is on day 1. incidenceComputation is then used to compute the number of incidents
# per interval. This result is then again shifted back to the original starting
# date, and transformed back into POSIXlt.

# Input:
#     SymptomOnset  - an nx1 POSIXlt vector, where n is the number of cases
#     Interval      - an integer (number of days in the interval)
# Output:
#     Incidents     - a named list with 2 members (Days [first day per interval]
#                     and NoOfCases [count of incidents during the respective
#                     interval])

incidence.POSIXlt <- function(SymptomOnset, Interval = 1, ...) {

  # Delete after testing and before creating package
  print("POSIXlt")

  # Dates are represented as the number of days since 1970-01-01
  # Transform SymptomOnset to numeric
  SymptomOnsetNumericTemp <- as.numeric(SymptomOnset) / (24 * 60 * 60)

  # Shift so that the first incidence is on day 1
  SymptomOnsetNumeric <- SymptomOnsetNumericTemp - min(SymptomOnsetNumericTemp) + 1

  # Use incidenceComputation to compute the incidents
  Incidents <- incidenceComputation(SymptomOnsetNumeric, Interval)

  # Shift data back to the original starting input value and original class
  Incidents$NoOfCases <- Incidents$NoOfCases
  Incidents$Days      <- as.POSIXlt((Incidents$Days + min(SymptomOnsetNumericTemp) - 1) * 24 * 60 * 60, origin = "1970-01-01")
  Incidents
  }
