incidence.integer <-
function(SymptomOnset, Interval=1, ...) {

  # Delete after testing and before creating package
  print("Integer")

  # Transform SymptomOnset to numeric
  SymptomOnsetNumericTemp <- as.numeric(SymptomOnset)

  # Shift data so that the first incidence is on day 1
  SymptomOnsetNumeric <- SymptomOnsetNumericTemp-min(SymptomOnsetNumericTemp)+1

  # Use incidenceComputation to compute the incidents
#  # Delete after testing
#  source("incidenceComputation.R")
  Incidents <- incidenceComputation(SymptomOnsetNumeric, Interval)

  # Shift data back to the starting value of the input and input class
  Incidents$NoOfCases <- as.integer(Incidents$NoOfCases)
  Incidents$Days <- as.integer(Incidents$Days+min(SymptomOnsetNumericTemp)-1)
  Incidents   }
