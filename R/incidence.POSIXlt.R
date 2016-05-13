incidence.POSIXlt <-
function(SymptomOnset, Interval = 1, ...) {

  # Delete after testing and before creating package
  print("POSIXlt")

  # Dates are represented as the number of days since 1970-01-01
  # Transform SymptomOnset to numeric
  SymptomOnsetNumericTemp <- as.numeric(SymptomOnset)/(24*60*60)

  # Shift so that the first incidence is on day 1
  SymptomOnsetNumeric <- SymptomOnsetNumericTemp-min(SymptomOnsetNumericTemp)+1

  # Use incidenceComputation to compute the incidents
#  # Delete after testing
#  source("incidenceComputation.R")
  Incidents <- incidenceComputation(SymptomOnsetNumeric, Interval)

  # Shift data back to the original starting input value and original class
  Incidents$NoOfCases <- Incidents$NoOfCases
  Incidents$Days      <- as.POSIXlt((Incidents$Days+min(SymptomOnsetNumericTemp)-1)*24*60*60, origin="1970-01-01")
  Incidents   }
