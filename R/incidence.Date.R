incidence.Date <-
function(SymptomOnset, Interval=1, ...) {

  # Delete after testing and before creating package
  print("Date")

  # Dates are represented as the number of days since 1970-01-01
  # Transform SymptomOnset to numeric
  SymptomOnsetNumericTemp <- as.numeric(SymptomOnset)

  # Move so that the first incidence is on day 1
  SymptomOnsetNumeric <- SymptomOnsetNumericTemp-
                          min(SymptomOnsetNumericTemp)+1

  # Use incidenceComputation to compute the incidents
#  # Delete after testing
#  source("incidenceComputation.R")
  Incidents <- incidenceComputation(SymptomOnsetNumeric, Interval)

  # Move data back to the original starting input value and original class
  Incidents$NoOfCases <- Incidents$NoOfCases
  Incidents$Days      <- as.Date(Incidents$Days+
                        min(SymptomOnsetNumericTemp)-1, origin="1970-01-01")
  Incidents   }
